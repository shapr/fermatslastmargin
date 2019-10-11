{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Config.Schema.Load
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.Map.Strict                      as M
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Time
import           Lucid                                (renderText)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse
import           System.Directory                     (createDirectoryIfMissing,
                                                       getHomeDirectory)
import           System.Exit                          (ExitCode (..))
import           System.FilePath                      ((</>))
import           Web.Scotty

import           Lib

localUserDir  = ".fermatslastmargin/localuser"

main :: IO ()
main = do
  userHomeDir <- getHomeDirectory
  let fullUserDir = userHomeDir </> ".fermatslastmargin/localuser"
      fullLocalDir = userHomeDir </> ".fermatslastmargin"
      fullStaticDir = userHomeDir </> ".fermatslastmargin/pageimages"

  -- load all papers and notes
  userState <- readState fullUserDir
  -- load github username and oauth token
  -- gc <- loadValueFromFile githubSpec "/home/shae/.fermatslastmargin/config"
  -- print gc
  -- friendState <- readState -- XXX
  scotty 3000 $ do
         middleware logStdoutDev
         middleware $ staticPolicy (noDots >-> addBase "static")
         middleware $ staticPolicy (noDots >-> addBase (fullLocalDir </> "pageimages"))

         get "/foo%2C" $ html "you got the url encoded option"
         get "/foo," $ html "you got the NOT YET url encoded option"
         get "/" $ do
                  nowTime <- liftIO getCurrentTime
                  userState <- liftIO $ readState (userHomeDir </> ".fermatslastmargin/localuser")
                  html . renderText $ pageTemplate "Papers" (papersadd (utctDay nowTime) >> notespush >> paperstable (M.elems userState))

         post "/paper" $ do
                  ps <- params
                  fs <- files
                  let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
                  let maybePaper = mbP ps
                  case maybePaper of Just thePaper -> do
                                       liftIO $ writeState fullUserDir $ M.insert (uid thePaper) thePaper userState
                                       liftIO $ commitEverything fullUserDir -- should have just written the paper.json, now stuff it into git
                                       let paperDir = fullStaticDir </> T.unpack (uid thePaper)
                                       liftIO $ createDirectoryIfMissing True paperDir -- gotta have this
                                       liftIO $ BS.writeFile (paperDir  </> "paper.pdf") (BSL.toStrict $ third $ head fs') -- head scares me, gonna die at some point
                                       liftIO $ renderPageImages paperDir
                                       liftIO $ print "should have worked now!"
                                       redirect "/"
                                     Nothing -> raise "something's broken"

         get "/annotate/:uid" $ do
                  uid <- param "uid"
                  redirect $ "/index.html?uid=" <> uid <> "&pagenum=1"

         get "/annotate/:uid/:pagenum" $ do -- smart thing to do is dump 'em all into the browser and load from javascript
                  pagenum <- param "pagenum"
                  uid <- param "uid"
                  userState <- liftIO $ readState fullUserDir
                  let mbPaper = M.lookup uid userState
                  final <- case mbPaper of
                             Nothing -> raise "That Paper does not exist"
                             Just p  -> pure $ maybe (Annotation "" pagenum uid) id (maybeGetAnnotation pagenum (notes p)) -- ugh!
                  json final

         post "/annotate/:uid/:pagenum" $ do
                  (jd :: Annotation) <- jsonData
                  (uid :: T.Text) <- param "uid"
                  pagenum <- param "pagenum"
                  userState <- liftIO $ readState fullUserDir
                  let mbPaper = M.lookup uid userState
                  final <- case mbPaper of
                             Nothing -> raise "That Paper does not exist"
                             Just p  -> liftIO $ writePaper fullUserDir $ p { notes = (upsertAnnotation jd (notes p))}
                  liftIO $ commitEverything fullUserDir
                  json final
                  html $ TL.pack $ show jd

                  redirect $ "/index.html?" <> pagenum

         post "/annotate" $ do
                  (jd :: Annotation) <- jsonData
                  userState <- liftIO $ readState fullUserDir
                  let puid = paperuid jd
                      mbPaper = M.lookup puid userState
                  final <- case mbPaper of
                             Nothing -> raise "That Paper does not exist"
                             Just p  -> liftIO $ writePaper fullUserDir $ p { notes = (upsertAnnotation jd (notes p))}
                  liftIO $ commitEverything fullUserDir
                  json final
         -- didn't see this coming, too bad DOI has forward slash that makes everything a huge pain
         post "/getannotate" $ do
                  (jd :: Annotation) <- jsonData
                  let pagenum = pageNumber jd
                      puid = paperuid jd
                  userState <- liftIO $ readState fullUserDir
                  let puid = paperuid jd
                      mbPaper = M.lookup puid userState
                  final <- case mbPaper of
                            Nothing -> raise "That Paper does not exist"
                            Just p  -> pure $ maybe (Annotation "" pagenum puid) id (maybeGetAnnotation pagenum (notes p)) -- ugh!
                  json final

         get "/gitpush" $ do
                  (exitCode, result) <- liftIO $ pushEverything fullUserDir
                  html $ case exitCode of
                           ExitSuccess -> "Successfully pushed to github"
                           _           -> "Failed to push to github"
