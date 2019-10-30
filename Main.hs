{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Config.Schema.Load
import           Control.Monad                        (unless, (<=<))
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Time
import           Lucid                                (renderText)
import           Network.HTTP.Client.TLS              (newTlsManager)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse
import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist,
                                                       getHomeDirectory)
import           System.Exit                          (ExitCode (..))
import           System.FilePath                      ((</>))
import           Web.Scotty

import           Lib

main :: IO ()
main = do
  userHomeDir <- getHomeDirectory
  let fullLocalDir = userHomeDir </> ".fermatslastmargin"
      fullUserDir = fullLocalDir </> "localuser"
      fullStaticDir = fullLocalDir </> "pageimages"
      fullFriendsDir = fullLocalDir </> "friends"

  -- create config dirs if missing
  mapM_ (createDirectoryIfMissing True) [fullUserDir, fullStaticDir, fullFriendsDir]
  -- create HTTP manager cause we gonna need it?
  mgmt <- newTlsManager
  -- load all papers and notes
  userState <- readState fullUserDir
  -- load github username and oauth token
  let configFile = fullLocalDir </> "config"
  haveConfigFile <- doesFileExist configFile
  unless haveConfigFile (writeFile configFile "username: \"\"\noauth: \"\"")
  gc <- loadValueFromFile githubSpec configFile
  -- gc <- loadValueFromFile githubSpec (fullLocalDir </> "config")
  friendState <- readFriendState fullFriendsDir
  -- M.Map paperUID [username] so the front end can easily display friends who have notes on this paper
  let friendPapers = friendView friendState
  -- print friendState
  scotty 3000 $ do
         middleware logStdoutDev
         middleware $ staticPolicy (noDots >-> addBase "static")
         middleware $ staticPolicy (noDots >-> addBase (fullLocalDir </> "pageimages"))

         -- this function always matches, then checks for empty gitconfig
         -- can't possibly be fast to check for EVERY SINGLE REQUEST, what's easier/simpler ?
         get (function $ const $ Just []) $ do
                  -- is this really a good idea?
                  gc <- liftIO $ loadValueFromFile githubSpec configFile
                  unless ("" == (username gc) || ("" == (oauth gc))) next
                  html . renderText $ pageTemplate "Set GitHub Configuration Values" authform

         get "/" $ do
                  nowTime <- liftIO getCurrentTime
                  userState <- liftIO $ readState (userHomeDir </> ".fermatslastmargin/localuser")
                  html . renderText $ pageTemplate "Papers" (papersadd (utctDay nowTime) >> notespush >> friendspull >> paperstable (M.elems userState))

         post "/setauth" $ do -- this isn't real secure
                  liftIO $ print "yeah, this really works"
                  username <- param "username"
                  oauth <- param "oauth"
                  -- https://github.com/glguy/config-schema/issues/2
                  liftIO $ writeFile configFile ("username: \"" <> username <> "\"\noauth: \"" <> oauth <> "\"")
                  gc <- liftIO $ loadValueFromFile githubSpec configFile
                  html "Your credentials have been saved"
                  redirect "/newuser"

         post "/paper" $ do
                  ps <- params
                  fs <- files
                  let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
                  let maybePaper = mbP ps
                  case maybePaper of Just thePaper -> do
                                       liftIO $ writeState fullUserDir $ M.insert (uid thePaper) thePaper userState
                                       liftIO $ commitEverything fullUserDir -- just wrote paper.json, now stuff it into git
                                       let paperDir = fullStaticDir </> T.unpack (uid thePaper)
                                       liftIO $ createDirectoryIfMissing True paperDir -- gotta have this
                                       liftIO $ BS.writeFile (paperDir  </> "paper.pdf") (BSL.toStrict $ third $ head fs') -- head scares me, gonna die at some point
                                       liftIO $ renderPageImages paperDir
                                       liftIO $ print "should have worked now!"
                                       redirect "/"
                                     Nothing -> raise "something's broken"

         post "/annotate" $ do
                  (jd :: Annotation) <- jsonData
                  userState <- liftIO $ readState fullUserDir
                  let puid = paperuid jd
                      mbPaper = M.lookup puid userState
                  final <- case mbPaper of
                             Nothing -> raise "That Paper does not exist"
                             Just p  -> liftIO $ writePaper fullUserDir $ p { notes = upsertAnnotation jd (notes p)}
                  liftIO $ commitEverything fullUserDir
                  json final

         -- didn't see this coming, too bad DOI has forward slash that makes everything a huge pain
         get "/getannotate" $ do -- this should really be a GET, not a POST
                  pagenum <- param "pagenum"
                  uid <- param "paperuid"
                  ps <- params
                  userState <- liftIO $ readState fullUserDir
                  -- point free code below replaces a big pile of pattern matches on Maybe!
                  let mbFriendnote = maybeGetAnnotation pagenum . notes -- that friend's paper have any notes for this page?
                                     <=< M.lookup uid -- does that friend have this paper?
                                     <=< flip M.lookup friendState . TL.toStrict -- does that friend exist?
                                     <=< lookup "viewfriend" $ ps -- is the user trying to view notes from a friend?
                      friendnote = maybe (Annotation "" pagenum uid) id mbFriendnote
                  let mbPaper = M.lookup uid userState
                  final <- case mbPaper of
                            Nothing -> raise "That Paper does not exist"
                            Just p  -> pure $ fromMaybe (Annotation "Press Enter to edit this note" pagenum uid) (maybeGetAnnotation pagenum (notes p)) -- ugh!
                  json [final,friendnote] -- return an array of localuser note, and selected friend note. There must be a better way, argh

         get "/friends" $ do
                  paperuid <- param "paperuid"
                  json $ M.findWithDefault [] paperuid friendPapers

         get "/gitpush" $ do
                  (exitCode, result) <- liftIO $ pushEverything fullUserDir
                  html $ case exitCode of
                           ExitSuccess -> "Successfully pushed to github"
                           _           -> "Failed to push to github"
         get "/gitpull" $ do
                  liftIO $ getFriendRepos (username gc) (oauth gc) fullFriendsDir mgmt
                  redirect "/" -- should probably report problems someday

         get "/newuser" $ do
                  -- create the repo on github
                  gc <- liftIO $ loadValueFromFile githubSpec configFile
                  createRes <- liftIO $ createDR (T.unpack $ oauth gc)
                  -- clone the repo from github into fullUserDir
                  case createRes of
                    Left e -> html $ TL.pack $ show e
                    Right r -> do
                            liftIO $ cloneRepo fullUserDir (oauthedremote . unstupid $ (snd . pnRepo) r)
                            html "new user repo cloned"
                                where oauthedremote = swizzle (username gc) (oauth gc)
