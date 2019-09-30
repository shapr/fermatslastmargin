{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (LoggingT, runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Trans.Resource
import qualified Data.Map.Strict                      as M
import           Data.Monoid                          (mconcat)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import qualified Data.Text.Lazy                       as TL
import           Data.Time
import           Lucid                                (renderText)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Directory                     (getHomeDirectory)
import           System.FilePath                      ((</>))
import           System.IO                            (stdout)
import           Web.Scotty

import           Lib
import qualified Lib

localUserDir  = ".fermatslastmargin/localuser"

main :: IO ()
main = do
  -- configContents <- TIO.readFile "~/.fermatslastmargin/config" -- needed once github stuff works
  userHomeDir <- getHomeDirectory
  let fullUserDir = userHomeDir </> ".fermatslastmargin/localuser"
      fullLocalDir = userHomeDir </> ".fermatslastmargin"
  userState <- readState fullUserDir
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
                  html . renderText $ pageTemplate "Papers" (papersadd (utctDay nowTime) >> paperstable (M.elems userState))
         post "/paper" $ do
                  ps <- params
                  let maybePaper = mbP ps
                  case maybePaper of Just thePaper -> do
                                       liftIO $ writeState (userHomeDir </> localUserDir) $ M.insert (uid thePaper) thePaper userState
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
