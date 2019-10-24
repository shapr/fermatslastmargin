{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Lib where

import           Config.Schema
import           Control.Monad         (filterM, join)
import           Control.Monad.Extra
import           Data.Aeson            (FromJSON, ToJSON, decodeStrict)
import           Data.Aeson.Text       (encodeToLazyText)
import qualified Data.ByteString       as BS
import qualified Data.Map.Strict       as M
import           Data.Maybe            (catMaybes, isJust, listToMaybe)
import           Data.Text             (Text, pack)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import qualified Data.Text.Lazy        as TL
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar    (Day)
import           GHC.Generics
import           Lib.Github
import           Lucid
import           Network.HTTP.Client   (Manager)
import           System.Directory
import           System.Exit           (ExitCode)
import           System.FilePath       (combine, splitFileName, (</>))
import           System.FilePath.Find  (always, fileName, find, (~~?))
import           System.FilePath.Manip (renameWith)
import           System.Process        (StdStream (..), close_fds,
                                        createProcess, cwd, proc, std_err,
                                        std_in, std_out, waitForProcess)
import           Text.Read
import           Web.Scotty            (Param)

-- | Map from DOI to Paper
type FLMState = M.Map Text Paper -- local user state

-- | Map from friend name to FLMState
type FriendState = M.Map Text FLMState

data Paper = Paper {
      uid       :: Text -- usually DOI
    , author    :: Text -- really needs to be [Text] at some point
    , published :: Day -- unpublished or pre-prints? (Maybe Day) instead?
    , title     :: Text
    , notes     :: ![Annotation]
    } deriving (Show, Generic, ToJSON, FromJSON)

data Annotation = Annotation {
      content    :: Text
    , pageNumber :: Int -- if this ever exceeds a 64 bit Int, something is very wrong
    , paperuid   :: Text -- this makes life much easier
    } deriving (Show, Generic, ToJSON, FromJSON)

-- read and save state
maybeGetPage pageNum anns = lookup pageNum annPairs
    where annPairs = zip (pageNumber <$> anns) (content <$> anns)

maybeGetAnnotation pageNum anns = lookup pageNum annPairs
    where annPairs = zip (pageNumber <$> anns) anns

upsertAnnotation :: Annotation -> [Annotation] -> [Annotation]
upsertAnnotation a@(Annotation c pnum puid) oldAnns = if doesExist then replaceAnnotation pnum c oldAnns else a:oldAnns
    where doesExist = isJust $ maybeGetPage pnum oldAnns


replaceAnnotation :: Int -> Text -> [Annotation] -> [Annotation]
replaceAnnotation i content [] = []
replaceAnnotation i content (a@(Annotation c p u):anns) = if p == i then Annotation content p u : anns else a : replaceAnnotation i content anns

-- | read the names of the directories in the config directory
readState :: FilePath -> IO FLMState
readState fp = do
  allfiles <- listDirectory fp
  uids <- filterDirectory $ fmap (fp </>) allfiles -- put dirname in front
  ps <- sequence $ readPaper <$> uids
  let ps' = catMaybes ps -- drop the Paper values that failed to decode
      -- TODO should I be using dir names? if I use uid from Paper elsewhere, use that instead! TODO
  return $ M.fromList $ zip (uid <$> ps') ps' -- set the unique ID as the key, the Paper as the value

-- | filepath should be the FULL path to the user dir, so either localuser or a friendname
writeState :: FilePath -> FLMState -> IO ()
writeState fp flms = do
  -- M.Map Text Paper , dirname / doi -> Paper
  _ <- createDirectoryIfMissing True fp -- create friend/user dir if needed
  print $ "fp is " <> fp <> " and FLMState is " <> show flms
  mapM_ (writePaper fp) (M.elems flms)

-- | given a directory for a paper, read that json file into a Paper value
readPaper :: FilePath -> IO (Maybe Paper)
readPaper fp = do
  f <- listToMaybe <$> findPaper fp "paper.json"
  case f of
    Nothing -> pure Nothing
    Just p  -> decodeStrict <$> BS.readFile p


-- | assume the dir given is the *USER* directory where all papers have their own directory
-- | arguments will be something like "~/.fermatslastmargin/localuser" and "10.4204/EPTCS.275.6"
-- | or perhaps "~/.fermatslastmargin/friends/pigworker" "10.4204/EPTCS.275.6"
-- | forward slash is not allowed in any filenames, so we substitute underscore _
writePaper :: FilePath -> Paper -> IO FilePath
writePaper fp p = do
  let fullDir = fp </> (T.unpack $ uid p)
  _ <- createDirectoryIfMissing True fullDir
  I.writeFile (fullDir </> "paper.json") (encodeToLazyText p)
  return fullDir

-- | given the friends dir, load FLM state from each of those dirs
readFriendState :: FilePath -> IO FriendState
readFriendState fp = do
  friendNames <- listDirectory fp
  friendDirs <- filterDirectoryPair $ zip (fmap (fp </>) friendNames) friendNames
  friendStates <- mapM readState (fst <$> friendDirs)
  pure $ M.fromList (zip (T.pack <$> friendNames) friendStates)

-- | front end code is easier if FriendState is converted to M.Map paperUID [username]
type FriendView = M.Map Text [Text]

-- ugh, code from tired brain, there must be a simpler way!
friendView :: FriendState -> FriendView
friendView fs = M.fromListWith (<>) (rewire (boph <$> M.toList fs))

boph (friendname,flmstate) = (friendname, M.keys flmstate)

rewire :: [(a,[b])] -> [(b,[a])]
rewire [] = []
rewire ((username,uids):friends) = zip uids (repeat [username]) <> rewire friends

-- what's wrong with the dang parser here?
filterDirectory :: [FilePath] -> IO [FilePath]
filterDirectory = filterM doesDirectoryExist

-- | [("/home/shae","shae"),("/home/plato","plato")] -> [("/home/shae","shae")]
-- I don't much like this, but I need it anyway
filterDirectoryPair :: [(FilePath, FilePath)] -> IO [(FilePath, FilePath)]
filterDirectoryPair = filterM (\(a,b) -> doesDirectoryExist a)

filterFile :: [FilePath] -> IO [FilePath]
filterFile = filterM doesFileExist

-- well this won't work anymore, will it?
getContent = content
getPaperId = uid

-- html page stuff

pageTemplate :: Monad m => Text -> HtmlT m a -> HtmlT m a
pageTemplate title content = do
  doctype_
  html_ $ do
    head_ $ do
      title_ $ toHtml title
      link_ [rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"]
    body_ content

papersadd :: Monad m => Day -> HtmlT m ()
papersadd nowTime = do
  form_ [action_ "/paper", method_ "post", enctype_ "multipart/form-data"] $
      do
        table_ $ do
          tr_ $ do
            td_ $ label_ "DOI"
            td_ $ input_ [type_ "text", name_ "doi"]
          tr_ $ do
            td_ $ label_ "Title"
            td_ $ input_ [type_ "text", name_ "title"]
          tr_ $ do
            td_ $ label_ "Authors"
            td_ $ input_ [type_ "text", name_ "author"]
          tr_ $ do
            td_ $ label_ "Publication Date"
            td_ $ input_ [type_ "text", name_ "pubdate", value_ (pack . show $ nowTime)]
          tr_ $ do
            td_ $ label_ "PDF of file"
            td_ $ input_ [type_ "file", name_ "uploadedfile"]
          tr_ $ do
            td_ $ input_ [type_ "submit"]

authform :: Monad m => HtmlT m ()
authform = do
  p_ "You will need to create a GitHub OAuth token, with scope \"public_repo\" please follow the instructions below:"
  a_ [href_ "https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line"] "Creating a personal access token for the command line"
  p_ ""
  form_ [action_ "/setauth", method_ "post"] $ do
             label_ "GitHub OAuth token"
             input_ [type_ "text", name_ "oauth"]
             label_ "GitHub username"
             input_ [type_ "text", name_ "username"]
             input_ [type_ "submit"]
  p_ "Please confirm that your global git config has settings for both user.email and user.name. You can explicitly set these values with:"
  code_ "git config --global user.name \"Mona Lisa\""
  p_ ""
  code_ "git config --global user.email \"mona@davinci.com\""


notespush :: Monad m => HtmlT m ()
notespush = a_ [href_ "/gitpush"] "Push notes to GitHub"

friendspull :: Monad m => HtmlT m ()
friendspull = a_ [href_ "/gitpull"] "Pull friends' notes from GitHub"

paperstable :: Monad m => [Paper] -> HtmlT m ()
paperstable rows =
  table_ $ do
    tr_ $
      th_ "Rows"
    sequence_ $ onepaper <$> rows

onepaper :: Monad m => Paper -> HtmlT m ()
onepaper r = tr_ $
  do td_ $ do
       a_ [href_ ("/index.html" <> "?pagenum=1" <> "&uid=" <> uid r)] (toHtml $ title r)
     tdit (T.pack . show . published :: Paper -> Text)
     tdit uid
     tdit author
          where tdit f = td_ . toHtml $ f r
                ruid = uid r


-- ?doi=10.25&title=this+is+a+title&author=Shae+Erisson&pubdate=2019-01-01
mbP :: [Param] -> Maybe Paper
mbP d = let upl = flip lookup d in
        do d <- upl "doi"
           t <- upl "title"
           a <- upl "author"
           pd <- upl "pubdate"
           mpd <- readMaybe $ TL.unpack pd -- there's got to be a better way
           return $ Paper (TL.toStrict d) (TL.toStrict a) mpd (TL.toStrict t) []

-- dunno if this is any better
mbP' :: [Param] -> Maybe Paper
mbP' ps = Paper
          <$> (supl "doi")
          <*> (supl "author")
          <*> (join $ readMaybe <$> (TL.unpack <$> upl "pubdate")) -- SO MUCH CHEESE, lifting everything to Maybe then joining?!
          <*> (supl "title")
          <*> Just []
    where upl = flip lookup ps
          supl a = TL.toStrict <$> upl a

-- find file in subdirs
findPaper :: FilePath -> FilePath -> IO [FilePath]
findPaper top match = find always (fileName ~~? match) top

-- random useful thing
third (a,b,c) = c

-- convert a file into static page images
renderPageImages :: FilePath -> IO (ExitCode, Text)
renderPageImages fp = do
  (Nothing, Nothing, Just errh, pid) <- createProcess (proc "pdftocairo" ["-png", "paper.pdf", "page"]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  exitCode <- waitForProcess pid
  result <- decodeUtf8 <$> BS.hGetContents errh
  renameZ fp
  return (exitCode, result)

-- f <- find always (fileName ~~? "page-*.png") "/home/shae/.fermatslastmargin/pageimages/10.4204/EPTCS.275.6"
-- splitFileName $ head f
-- ("/home/shae/.fermatslastmargin/pageimages/10.4204/EPTCS.275.6/","page-01.png")
renameZ :: FilePath -> IO ()
renameZ fp = do
  names <- find always (fileName ~~? "page-*.png") fp
  mapM_ (renameWith changeWholePath) names

-- | should convert like this: foo/bar/page-0001.png -> foo/bar/page-1.png
changeWholePath fp =  uncurry combine . fixName $ splitFileName fp
    where fixName = \(x,y)-> (x,fixZ y)

-- ugly, but works, kinda?
fixZ n@('p':'a':'g':'e':'-':xs) = "page-" <> killZeroes xs
fixZ n                          = n

killZeroes ('0':xs) = killZeroes xs
killZeroes x        = x

-- github config
data GithubConfig = GC {
      username :: Text
    , oauth    :: Text
    } deriving (Show, Eq, Ord)

githubSpec :: ValueSpec GithubConfig
githubSpec = sectionsSpec "github" $
         do username <- reqSection "username" "GitHub username"
            oauth <- reqSection "oauth" "OAuth Token for GitHub"
            pure GC{..}

-- whole buncha shelling out to git to save, push, or pull friends repos

-- pitch everything into git!
commitEverything :: FilePath -> IO (ExitCode, Text)
commitEverything fp = do
  (Nothing, Nothing, Just errh, pid) <- createProcess (proc "git" ["add", "-A"]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  exitCode <- waitForProcess pid
  (Nothing, Nothing, Just errhc, pidc) <- createProcess (proc "git" ["commit", "-m", "added understanding"]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  exitCode <- waitForProcess pidc
  result <- decodeUtf8 <$> BS.hGetContents errhc
  return (exitCode, result)

-- push to remote
pushEverything :: FilePath -> IO (ExitCode, Text)
pushEverything fp = do
  (Nothing, Nothing, Just errhc, pidc) <- createProcess (proc "git" ["push", "origin"]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  exitCode <- waitForProcess pidc
  result <- decodeUtf8 <$> BS.hGetContents errhc
  return (exitCode, result)

-- | Given a directory and git url, clone the git repo into that directory.
cloneRepo :: FilePath -> Text -> IO (ExitCode, Text)
cloneRepo fp url = do
  print $ "cloning " <> T.pack fp <> " from " <> url
  _ <- createDirectoryIfMissing True fp
  (Nothing, Nothing, Just errhc, pidc) <- createProcess (proc "git" ["clone",T.unpack url, fp]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  exitCode <- waitForProcess pidc
  result <- decodeUtf8 <$> BS.hGetContents errhc
  return (exitCode, result)

-- | Given a directory, run git pull in that directory.
pullRepo :: FilePath -> IO (ExitCode, Text)
pullRepo fp = do
  print $ "pulling " <> fp
  (Nothing, Nothing, Just errhc, pidc) <- createProcess (proc "git" ["pull"]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  exitCode <- waitForProcess pidc
  result <- decodeUtf8 <$> BS.hGetContents errhc
  return (exitCode, result)

-- dammit github, why is your API broke? oh I think I'm using the wrong field should be repoHtmlUrl? XXX fix this later!
unstupid = T.replace "/repos" "" . T.replace "api." ""

swizzle username oauth = T.replace "https://" ("https://" <> username <> ":" <> oauth)

-- | returns IO [(username, https url to flmdata)]
getFriendRepos :: Text -> Text -> String -> Manager -> IO ()
getFriendRepos username token friendsdir mgmt = do
  nameurlpairs <- findRepos' username token mgmt
  let friendDirs = (\(x,y) -> (friendsdir </> T.unpack x, unstupid y)) <$> nameurlpairs
  -- check for existence of the friendDirs ([yes], [no])
  (needpulls, needclones) <- partitionM (doesDirectoryExist . fst) friendDirs
  pullResults <- mapM pullRepo (fst <$> needpulls)
  cloneResults <- mapM (uncurry cloneRepo) needclones
  print $ show (length needpulls) <> " repos pulled, " <> show (length needclones) <> " new repos cloned."
  print $ "possible errors: " <> show pullResults <> show cloneResults

-- new user code
-- | Given a directory, run git pull in that directory.
-- checkGitConfig :: FilePath -> IO (ExitCode, Text)
checkGitConfig value = do -- value should be either "user.name" or "user.email"
  (Nothing, Nothing, Nothing, pidc) <- createProcess (proc "git" ["config",value]) { cwd = Just ".", std_in = NoStream, std_out = NoStream, std_err = NoStream, close_fds = True}
  exitCode <- waitForProcess pidc
  return exitCode

-- looks like names imported from Lib.Github are not automatically exported? who knew?!
createDR = createDataRepo
pnRepo = pairNameRepo
