{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Lib where

import           Config
import           Config.Schema
import           Config.Schema.Load
import           Control.Monad      (filterM, join, liftM)
import           Data.Aeson         (FromJSON, ToJSON, decodeStrict)
import           Data.Aeson.Text    (encodeToLazyText)
import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as M
import           Data.Maybe         (catMaybes)
import           Data.String        (IsString)
import           Data.Text          (Text, pack)
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import           Data.Text.Lazy.IO  as I
import           Data.Time.Calendar (Day)
import           Data.Time.Clock    (UTCTime)
import           GHC.Generics
import           Lucid
import           System.Directory
import           System.FilePath    ((</>))
import           Text.Read
import           Web.Scotty         (Param)

-- | DOI -> PageNumber -> Annotation
data FLMState = FS [Paper] (M.Map Text (M.Map Int Text)) -- local user state

-- | name of friend  -> FLMState
type FriendState = M.Map Text (FLMState)

data Paper = Paper {
      uid       :: Text -- usually DOI
    , author    :: Text -- really need to be [Text] at some point
    , published :: Day
    , title     :: Text
    } deriving (Show, Generic, ToJSON, FromJSON)
data Annotation = Annotation {
      content    :: Text
    , pageNumber :: Int -- if this ever exceeds a 64 bit Int, something is very wrong
    } deriving (Show, Generic, ToJSON, FromJSON)

-- read and save state

-- | read friend state,
readFriendState :: FilePath -> IO FriendState
readFriendState fp = do
  allfiles <- listDirectory fp
  name <- filterDirectory allfiles
  state <- mapM readState $ fmap (fp </>) name
  return $ M.fromList $ zip (T.pack <$> name) state

-- | read the names of the directories in the config directory
readState :: FilePath -> IO FLMState
readState fp = do
  allfiles <- listDirectory fp
  uids <- filterDirectory $ fmap (fp </>) allfiles -- put dirname in front
  let fulldirs = fmap (fp </>) uids
  ps <- sequence $ readPaper <$> fulldirs
  return $ M.fromList $ zip (T.pack <$> uids) ps

readPaper :: FilePath -> IO (M.Map Int Text)
readPaper fp = do
  allfiles <- listDirectory fp
  filenames <- filterFile $ fmap (fp </>) allfiles
  bs <- mapM BS.readFile filenames
  let mbAnnotations = decodeStrict <$> bs
  return $ buildPaperState (catMaybes mbAnnotations)

buildPaperState :: [Annotation] -> M.Map Int Text
buildPaperState as = M.fromList $ (\(Annotation c p) -> (p,c)) <$> as

filterDirectory :: [FilePath] -> IO [FilePath]
filterDirectory = filterM doesDirectoryExist

filterFile :: [FilePath] -> IO [FilePath]
filterFile = filterM doesFileExist

-- well this won't work anymore, will it?
getContent = content
getPaperId = uid

data GithubConfig = GC {
      username :: Text
    , oauth    :: Text
    } deriving (Show, Eq, Ord)

githubSpec :: ValueSpec GithubConfig
githubSpec = sectionsSpec "github" $
         do username <- reqSection "username" "GitHub username"
            oauth <- reqSection "oauth" "OAuth Token for GitHub"
            pure GC{..}


-- html page stuff

pageTemplate :: Monad m => Text -> HtmlT m a -> HtmlT m a
pageTemplate title content = do
  doctype_
  html_ $ do
    head_ $
      title_ $ toHtml title
    body_ content

papersadd :: Monad m => Day -> HtmlT m ()
papersadd nowTime = (do
  form_ [action_ "/paper", method_ "post"] $ do
              label_ "DOI"
              input_ [type_ "text", name_ "doi"]
              label_ "Title"
              input_ [type_ "text", name_ "title"]
              label_ "Authors"
              input_ [type_ "text", name_ "authors"]
              label_ "Publication Date"
              input_ [type_ "text", name_ "pubdate", value_ (pack . show $ nowTime)]
              input_ [type_ "submit"]
  )

paperstable :: Monad m => [Paper] -> HtmlT m ()
paperstable rows =
  table_ $ do
    tr_ $
      th_ "Rows"
    sequence_ $ onepaper <$> rows

onepaper :: Monad m => Paper -> HtmlT m ()
onepaper r = tr_ $
  do -- td_ . toHtml
     tdit title
     tdit $ (T.pack . show . published :: Paper -> Text)
     tdit uid
     tdit author
          where tdit f = td_ . toHtml $ f r

mbP :: [Param] -> Maybe Paper
mbP d = let upl = flip lookup d in
        do d <- upl "doi"
           t <- upl "title"
           a <- upl "author"
           pd <- upl "pubdate"
           mpd <- readMaybe $ TL.unpack pd -- there's got to be a better way
           return $ Paper (TL.toStrict d) (TL.toStrict a) mpd (TL.toStrict t)

-- dunno if this is any better
mbP' :: [Param] -> Maybe Paper
mbP' ps = Paper
          <$> (supl "doi")
          <*> (supl "author")
          <*> (join $ readMaybe <$> (TL.unpack <$> upl "pubdate")) -- SO MUCH CHEESE, lifting everything to Maybe then joining?!
          <*> (supl "title")
    where upl = flip lookup ps
          supl a = TL.toStrict <$> (upl a)
