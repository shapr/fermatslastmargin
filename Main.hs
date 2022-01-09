{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Config.Schema.Load
import Control.Monad (unless, void, (<=<))
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Lib
import Lib.Github (validateAuthToken)
import Lucid (renderText)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (badRequest400)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    findExecutable,
    getHomeDirectory,
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Web.Scotty

main :: IO ()
main = do
  userHomeDir <- getHomeDirectory
  let fullLocalDir = userHomeDir </> ".fermatslastmargin"
      fullUserDir = fullLocalDir </> "localuser"
      fullStaticDir = fullLocalDir </> "pageimages"
      fullFriendsDir = fullLocalDir </> "friends"

  -- create config dirs if missing
  mapM_ (createDirectoryIfMissing True) [fullUserDir, fullStaticDir, fullFriendsDir]
  -- check for pdftocairo in $PATH, or crap out and die
  ifM (isJust <$> findExecutable "pdftocairo") (pure ()) (error "cannot find pdftocairo in $PATH")
  -- create HTTP manager cause we gonna need it?
  mgmt <- newTlsManager
  -- load all papers and notes
  userState <- readState fullUserDir
  -- load github username and oauth token
  let configFile = fullLocalDir </> "config"
  haveConfigFile <- doesFileExist configFile
  unless haveConfigFile (writeFile configFile "username: \"\"\noauth: \"\"")
  gc <- loadValueFromFile githubSpec configFile
  friendState <- readFriendState fullFriendsDir
  friendPapers <- readFriendView fullFriendsDir
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ staticPolicy (noDots >-> addBase (fullLocalDir </> "pageimages"))

    -- this function always matches, then checks for empty gitconfig
    -- can't possibly be fast to check for EVERY SINGLE REQUEST, what's easier/simpler ?
    get (function $ const $ Just []) $ do
      -- is this really a good idea?
      gc' <- liftIO $ loadValueFromFile githubSpec configFile
      unless ("" == username gc || ("" == oauth gc')) next
      html . renderText $ pageTemplate "Set GitHub Configuration Values" authform

    get "/" $ do
      nowTime <- liftIO getCurrentTime
      papers <- liftIO $ readPaper fullUserDir
      html . renderText $
        pageTemplate "Papers" $ do
          flmheader -- Render header
          papersearch -- Render paper search form
          notespush -- Render "Push Notes" button
          friendspull -- Render "Pull Friends Notes" button
          paperfilterForm -- Render paper filter form
          paperstable papers -- Render papers table
          papersadd (utctDay nowTime) -- Render add paper form
    get "/filter" $ do
      (searchTerm :: T.Text) <- param "pattern"
      nowTime <- liftIO getCurrentTime
      papers <- liftIO $ filterpapers searchTerm <$> readPaper fullUserDir
      html . renderText $
        pageTemplate "Papers" $ do
          flmheader -- Render header
          papersearch -- Render paper search form
          notespush -- Render "Push Notes" button
          friendspull -- Render "Pull Friends Notes" button
          paperfilterForm -- Render paper filter form
          paperstable papers -- Render papers table
          papersadd (utctDay nowTime) -- Render add paper form
    post "/setauth" $ do
      -- this isn't real secure
      uname <- param "username" -- don't shadow username, it's a record accessor for GithubConfig
      mAuthToken <- validateAuthToken . T.strip <$> param "oauth"
      case mAuthToken of
        Nothing -> raiseStatus badRequest400 "Invalid github authentication token"
        Just authToken -> do
          -- https://github.com/glguy/config-schema/issues/2
          liftIO $ writeFile configFile ("username: \"" <> uname <> "\"\noauth: \"" <> T.unpack authToken <> "\"")
          void . liftIO $ loadValueFromFile githubSpec configFile
          html "Your credentials have been saved"
          redirect "/newuser"

    post "/paper" $ do
      ps <- params
      fs <- files
      let fs' = [(fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName, fi) <- fs]
      let maybePaper = mbP ps
      case maybePaper of
        Just thePaper -> do
          let cleanPaper = sanitizePaper thePaper
          liftIO $ do
            writeState fullUserDir $ M.insert (uid cleanPaper) cleanPaper userState
            _ <- commitEverything fullUserDir -- just wrote paper.json, now stuff it into git, don't even check it!
            let paperDir = fullStaticDir </> T.unpack (uid cleanPaper)
            createDirectoryIfMissing True paperDir -- gotta have this
            BS.writeFile (paperDir </> "paper.pdf") (BSL.toStrict $ third $ head fs') -- head scares me, gonna die at some point
            _ <- renderPageImages paperDir -- should really check/report failure here
            putStrLn "should have worked now!"
          redirect "/"
        Nothing -> raise "something's broken"

    post "/updatepdf" $ do
      puid <- param "uidtoupdate"
      fs <- files
      let fs' = [(fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName, fi) <- fs]
      _ <- liftIO $ do
        let paperDir = fullStaticDir </> TL.unpack puid
        createDirectoryIfMissing True paperDir -- gotta have this
        BS.writeFile (paperDir </> "paper.pdf") (BSL.toStrict $ third $ head fs') -- head scares me, gonna die at some point
        renderPageImages paperDir
      redirect $ "/index.html?uid=" <> puid

    post "/annotate" $ do
      (jd :: Annotation) <- jsonData
      mbPaper <- liftIO $ M.lookup (paperuid jd) <$> readState fullUserDir
      final <- case mbPaper of
        Nothing -> raise "That Paper does not exist"
        Just p -> liftIO $ writePaper fullUserDir $ p {notes = upsertAnnotation jd (notes p)}
      _ <- liftIO $ commitEverything fullUserDir
      json final

    get "/getannotate" $ do
      pagenum <- param "pagenum"
      puid <- param "paperuid"
      ps <- params
      mbPaper <- liftIO $ M.lookup puid <$> readState fullUserDir
      -- point free code below replaces a big pile of pattern matches on Maybe!
      let mbFriendnote =
            maybeGetAnnotation pagenum . notes -- that friend's paper have any notes for this page?
              <=< M.lookup puid -- does that friend have this paper?
              <=< flip M.lookup friendState . TL.toStrict -- does that friend exist?
              <=< lookup "viewfriend"
              $ ps -- is the user trying to view notes from a friend?
          friendnote = fromMaybe (Annotation "" pagenum puid) mbFriendnote
      final <- case mbPaper of
        Nothing -> raise "That Paper does not exist"
        Just p -> pure $ fromMaybe (Annotation "Press Enter to edit this note" pagenum puid) (maybeGetAnnotation pagenum (notes p)) -- ugh!
      json [final, friendnote] -- return an array of localuser note, and selected friend note. There must be a better way, argh
    get "/friends" $ do
      puid <- param "paperuid"
      json $ M.findWithDefault [] puid friendPapers

    get "/gitpush" $ do
      (exitCode, _) <- liftIO $ pushEverything fullUserDir
      case exitCode of
        ExitSuccess -> redirect "/"
        _ -> html "Failed to push to github"
    get "/gitpull" $ do
      liftIO $ getFriendRepos (username gc) (oauth gc) fullFriendsDir mgmt
      redirect "/" -- should really report problems someday
    get "/crossref" $ do
      (terms :: T.Text) <- param "searchterms"
      -- https://github.com/CrossRef/rest-api-doc#api-overview
      -- wget "https://api.crossref.org/works?query=room+at+the+bottom"
      req <- parseRequest $ T.unpack ("http://api.crossref.org/works?query=" <> terms)
      response <- liftIO $ httpLbs req mgmt
      let wrapper = fromMaybe emptyWrapper (decodeSR $ responseBody response)
          foundpapers = converter <$> (items . message) wrapper
      html . renderText $ pageTemplate "Search Results" (foundpaperstable foundpapers)

    post "/newpaper" $ do
      newPapers :: [Paper] <- jsonData
      liftIO $ do
        let cleanPapers = sanitizePaper <$> newPapers
        print cleanPapers
        userState' <- readState fullUserDir
        print userState'
        let newState = addFoundPapers userState cleanPapers
        print newState
        writeState fullUserDir newState
      redirect "/"

    get "/newuser" $ do
      -- create the repo on github
      createRes <- liftIO $ loadValueFromFile githubSpec configFile >>= createDR . T.unpack . oauth
      -- clone the repo from github into fullUserDir
      case createRes of
        Left e -> html $ "There's a problem: " <> TL.pack (show e)
        Right r -> do
          _ <- liftIO $ cloneRepo fullUserDir (oauthedremote . unstupid $ (snd . pnRepo) r)
          redirect "/"
          where
            oauthedremote = swizzle (username gc) (oauth gc)

    get "/editmetadata" $ do
      (puid :: T.Text) <- param "uidtoupdate"
      mbPaper <- liftIO $ M.lookup puid <$> readState fullUserDir
      case mbPaper of
        Nothing -> html "That Paper does not exist"
        Just p -> html . renderText $ pageTemplate "Edit metadata" (paperedit p)

    post "/editmetadata" $ do
      (updatedpaper :: Paper) <- jsonData
      liftIO $ print ("it did not sh*t itself" <> show updatedpaper)
      mbPaper <- liftIO $ M.lookup (uid updatedpaper) <$> readState fullUserDir
      case mbPaper of
        Nothing -> do
          liftIO $ putStrLn "could not find the paper"
          html "That Paper does not exist"
        Just p -> do
          void . liftIO $
            writePaper fullUserDir $
              p
                { uid = uid updatedpaper,
                  author = author updatedpaper,
                  published = published updatedpaper,
                  notes = notes p -- don't modify the notes, copy from the previous Paper value
                }
          liftIO $ putStrLn "paper written successfully"
          redirect "/"
