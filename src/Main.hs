{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (LoggingT, runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Trans.Resource
import           Data.Monoid                          (mconcat)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import qualified Data.Text.Lazy                       as TL
import           Data.Time
import           Lucid                                (renderText)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.IO                            (stdout)
import           Web.Scotty

import           Lib
import qualified Lib

main :: IO ()
main = do
  -- configContents <- TIO.readFile "~/.fermatslastmargin/config"

  -- read state here
  userState <- readState "~/.fermatslastmargin/localuser"
  friendState <- readState
  print "userState is " <> show userState
  scotty 3000 $ do
         middleware $ staticPolicy (noDots >-> addBase "static")
         middleware logStdoutDev
         -- get "/" $ do
         --          html "HI THERE THIS GONNA WORK SOON"
         get "/" $ do
                  nowTime <- liftIO getCurrentTime
                  html . renderText $ pageTemplate "Papers" (papersadd (utctDay nowTime) >> paperstable papers)
         -- get "/:word" $ do
         --               beam <- param "word"
         --               html $ mconcat ["<html><h1>This is page number ", beam, "</h1></html>"]
         -- post "/jsonpaper" $
         --      do (p :: Paper) <- jsonData
         --         -- doi :: T.Text <- param "doi"
         --         pid <- liftIO $ runDb $ insert p
         --         html $ TL.pack $ show pid
         -- post "/paper" $ do
         --          ps <- params
         --          let maybePaper = mbP ps
         --          case maybePaper of Just thePaper -> do
         --                               _ <- liftIO $ runDb $ insert thePaper
         --                               redirect "/"
         --                             Nothing -> raise "something's broken"

         -- post "/annotate/:uid/:pagenum" $
         --      do (jd :: Annotation) <- jsonData
         --         (uid :: T.Text) <- param "uid"
         --         pagenum <- param "pagenum"
         --         paper <- liftIO $ runDb $ selectFirst [PaperDoi ==. uid] [] -- Maybe Paper or some such
         --         -- let k = case paper of Nothing -> raise "That Paper does not exist"
         --         --                       Just k -> getPKey k XXX stopped here XXX
         --         -- trying to fake up an Annotation since I don't have an easy way to stuff the db id into the form itself
         --         -- let uglyhack = jd {
         --         liftIO $ runDb $ upsert jd [AnnotationContent =. getContent jd]
         --         html $ TL.pack $ show jd

         --         redirect $ "/index.html?" <> pagenum
         -- get "/annotate/:uid/:pagenum" $ do -- smart thing to do is dump 'em all into the browser and load from javascript
         --     pagenum <- param "pagenum"
         --     uid <- param "uid"
         --     paper <- liftIO $ runDb $ selectFirst [PaperDoi ==. uid] []
         --     -- jd <- liftIO $ runDb $ selectFirst [AnnotationPaper ==. uid, AnnotationPageNumber ==. pagenum] []
         --     final <- case paper of Nothing -> raise "That Paper does not exist"
         --                            Just k -> liftIO $ runDb $ selectFirst [AnnotationPaper ==. getPKey k, AnnotationPageNumber ==. pagenum] [] -- ARGH SO UGLY
         --     json final
