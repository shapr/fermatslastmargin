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
  -- configContents <- TIO.readFile "~/.fermatslastmargin/config"

  -- read state here
  userHomeDir <- getHomeDirectory
  let fullUserDir = userHomeDir </> ".fermatslastmargin/localuser"
  userState <- readState fullUserDir -- I don't think I need this?
  -- friendState <- readState -- XXX
  -- print $ "userState is " <> show userState
  scotty 3000 $ do
         middleware $ staticPolicy (noDots >-> addBase "static")
         middleware logStdoutDev

         get "/" $ do
                  nowTime <- liftIO getCurrentTime
                  userState <- liftIO $ readState (userHomeDir </> ".fermatslastmargin/localuser")
                  -- papers <- ??
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
         -- post "/jsonpaper" $
         --      do (p :: Paper) <- jsonData
         --         -- doi :: T.Text <- param "doi"
         --         pid <- liftIO $ runDb $ insert p
         --         html $ TL.pack $ show pid
