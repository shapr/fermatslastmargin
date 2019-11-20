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
import           Data.Aeson            (FromJSON (..), ToJSON, Value (..),
                                        decode, decodeStrict, (.:), (.:?))
import           Data.Aeson.Text       (encodeToLazyText)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Char             (toLower)
import           Data.List             (intersperse)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (catMaybes, fromMaybe, isJust)
import           Data.Text             (Text, pack)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import qualified Data.Text.Lazy        as TL
import           Data.Text.Lazy.IO     as I
import           Data.Time.Calendar    (Day, fromGregorian)
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

emptyPaper :: Paper
emptyPaper = Paper "" "" (read "1000-01-01") "" []

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
maybeGetPage :: Int -> [Annotation] -> Maybe Text
maybeGetPage pageNum anns = lookup pageNum annPairs
    where annPairs = zip (pageNumber <$> anns) (content <$> anns)

maybeGetAnnotation :: Int -> [Annotation] -> Maybe Annotation
maybeGetAnnotation pageNum anns = lookup pageNum annPairs
    where annPairs = zip (pageNumber <$> anns) anns

upsertAnnotation :: Annotation -> [Annotation] -> [Annotation]
upsertAnnotation a@(Annotation c pnum _) oldAnns = if doesExist then replaceAnnotation pnum c oldAnns else a:oldAnns
    where doesExist = isJust $ maybeGetPage pnum oldAnns

replaceAnnotation :: Int -> Text -> [Annotation] -> [Annotation]
replaceAnnotation _ _ [] = []
replaceAnnotation i content (a@(Annotation _ p u):anns) = if p == i then Annotation content p u : anns else a : replaceAnnotation i content anns

-- | read the names of the directories in the config directory
readState :: FilePath -> IO FLMState
readState fp = do
  allfiles <- listDirectory fp
  toplevel <- filterDirectory $ fmap (fp </>) allfiles -- put dirname in front
  ps <- sequence $ readPaper <$> toplevel
  let ps' = concat ps -- drop the Paper values that failed to decode
      -- TODO should I be using dir names? if I use uid from Paper elsewhere, use that instead! TODO
  return $ M.fromList $ zip (uid <$> ps') ps' -- set the unique ID as the key, the Paper as the value

-- | filepath should be the FULL path to the user dir, so either localuser or a friendname
writeState :: FilePath -> FLMState -> IO ()
writeState fp flms = do
  let sanefp = toLower <$> fp
  -- M.Map Text Paper , dirname / doi -> Paper
  _ <- createDirectoryIfMissing True sanefp -- create friend/user dir if needed
  print $ "fp is " <> fp <> " and FLMState is " <> show flms
  mapM_ (writePaper fp) (M.elems flms)

-- | given a directory for an organization, read any paper.json files into Paper values
readPaper :: FilePath -> IO [Paper]
readPaper fp = do
  fns <- findPaper fp "paper.json"
  mbPs <- mapM (fmap decodeStrict . BS.readFile) fns
  pure $ catMaybes mbPs
  -- mapM (decodeStrict . BS.readFile) fns
  -- pure $ concat $ decodeStrict <$> fs
  -- case f of
  --   Nothing -> pure Nothing
  --   Just p  -> decodeStrict <$> BS.readFile p

-- | assume the dir given is the *USER* directory where all papers have their own directory
-- | arguments will be something like "~/.fermatslastmargin/localuser" and "10.4204/EPTCS.275.6"
-- | or perhaps "~/.fermatslastmargin/friends/pigworker" "10.4204/EPTCS.275.6"
writePaper :: FilePath -> Paper -> IO FilePath
writePaper fp p = do
  let fullDir = fp </> T.unpack (uid p)
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

boph :: (a1, M.Map k a2) -> (a1, [k])
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
filterDirectoryPair = filterM (\(a,_) -> doesDirectoryExist a)

filterFile :: [FilePath] -> IO [FilePath]
filterFile = filterM doesFileExist

-- html page stuff
flmheader :: Monad m => HtmlT m ()
flmheader = h1_ [class_ "site-title"] "Fermat's Last Margin"

pageTemplate :: Monad m => Text -> HtmlT m a -> HtmlT m a
pageTemplate title content = do
  doctype_
  html_ $ do
    head_ $ do
      title_ $ toHtml title
      link_ [rel_ "stylesheet", href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"]
      script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js",type_ "text/javascript"] ("" :: T.Text)
      script_ [src_ "/search.js",type_ "text/javascript"] ("" :: T.Text)
      link_ [rel_ "stylesheet", href_ "style.css"]
    body_ content

papersadd :: Monad m => Day -> HtmlT m ()
papersadd nowTime = do
  h2_ [class_ "page-title"] "Manually add paper metadata"
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
            td_ $ input_ [type_ "date", name_ "pubdate", value_ (pack . show $ nowTime)]
          tr_ $ do
            td_ $ label_ "PDF of file"
            td_ $ input_ [type_ "file", name_ "uploadedfile"]
          tr_ $ do
            td_ $ span_ ""
            td_ $ input_ [type_ "submit"]

paperedit :: Monad m => Paper -> HtmlT m ()
paperedit editpaper = do
  h2_ [class_ "page-title"] "Edit paper metadata"
  form_ [action_ "/paper", method_ "post", enctype_ "multipart/form-data"] $
      do
        table_ $ do
          tr_ $ do
            td_ $ label_ "DOI"
            td_ $ label_ [id_ "doi"] (toHtml $ uid editpaper)
          tr_ $ do
            td_ $ label_ "Title"
            td_ $ input_ [id_ "title", type_ "text", name_ "title", value_ (title editpaper)]
          tr_ $ do
            td_ $ label_ "Authors"
            td_ $ input_ [id_ "author", type_ "text", name_ "author", value_ (author editpaper)]
          tr_ $ do
            td_ $ label_ "Publication Date"
            td_ $ input_ [id_ "published", type_ "date", name_ "pubdate", value_ (pack . show $ published editpaper)]
          tr_ $ do
            td_ $ span_ ""
            td_ $ button_ [type_ "button", id_ "editmetadata"] "Apply changes"

papersearch :: Monad m => HtmlT m ()
papersearch = do
  h2_ [class_ "page-title"] "Search for paper metadata to add"
  form_ [action_ "/crossref", method_ "get"] $ do
      table_ [class_ "crossref-form"]$ do
                tr_ $ do
                  td_ $ label_ "Title words"
                  td_ $ input_ [type_ "text", name_ "searchterms"]
                  td_ $ input_ [type_ "submit", value_ "Search"]

authform :: Monad m => HtmlT m ()
authform = do
  p_ "You will need to create a GitHub OAuth token, with scope \"public_repo\" please follow the instructions below:"
  a_ [href_ "https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line", target_ "_blank"] "Creating a personal access token for the command line"
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
notespush = a_ [href_ "/gitpush", class_ "git gitpush"] "Push notes to GitHub"

friendspull :: Monad m => HtmlT m ()
friendspull = a_ [href_ "/gitpull", class_ "git gitpull"] "Pull friends' notes from GitHub"

paperstable :: Monad m => [Paper] -> HtmlT m ()
paperstable rows =
  table_ [class_ "paperlist"] $ do
    thead_ $
      tr_ $ do
        th_ "Paper Title"
        th_ "Edit"
        th_ "Pub Date"
        th_ "DOI"
        th_ "Authors"
    sequence_ $ viewPaper <$> rows

viewPaper :: Monad m => Paper -> HtmlT m ()
viewPaper r = tr_ $
  do td_ $ a_ [href_ ("/index.html" <> "?pagenum=1" <> "&uid=" <> uid r)] (toHtml $ title r) -- link to page view for paper
     td_ $ a_ [href_ ("/editmetadata?uidtoupdate=" <> uid r)] "Edit"
     tdit (T.pack . show . published :: Paper -> Text)
     tdit uid
     tdit author
          where tdit f = td_ . toHtml $ f r

foundpaperstable :: Monad m => [Paper] -> HtmlT m ()
foundpaperstable rows = do
  table_ [class_ "paperlist"] $ do
    thead_ $
      tr_ $ do
        th_ "Save?"
        th_ "Paper Title"
        th_ "Pub Date"
        th_ "DOI"
        th_ "Authors"
    sequence_ $ foundPaper <$> rows
  input_ [id_ "save", type_ "submit", value_ "Save Selected Papers"]

foundPaper :: Monad m => Paper -> HtmlT m ()
foundPaper r = tr_ $
  do td_ $ do
       input_ [name_ "selectedpaper", type_ "checkbox", value_ (TL.toStrict $ encodeToLazyText r)]
       -- input_ [id_ "papervalue", type_ "hidden", ]
     td_ $ a_ [href_ ("https://doi.org/" <> uid r), target_ "_blank"] (toHtml $ title r)
     tdit (T.pack . show . published :: Paper -> Text)
     tdit uid
     tdit author
          where tdit f = td_ . toHtml $ f r

-- ?doi=10.25&title=this+is+a+title&author=Shae+Erisson&pubdate=2019-01-01
mbP :: [Param] -> Maybe Paper
mbP d = let upl = flip lookup d in
        do doi <- upl "doi"
           t <- upl "title"
           a <- upl "author"
           pd <- upl "pubdate"
           mpd <- readMaybe $ TL.unpack pd -- there's got to be a better way
           return $ Paper (TL.toStrict doi) (TL.toStrict a) mpd (TL.toStrict t) []

-- dunno if this is any better
mbP' :: [Param] -> Maybe Paper
mbP' ps = Paper
          <$> supl "doi"
          <*> supl "author"
          <*> (join $ readMaybe =<< (TL.unpack <$> upl "pubdate")) -- SO MUCH CHEESE, lifting everything to Maybe then joining?!
          <*> supl "title"
          <*> Just []
    where upl = flip lookup ps
          supl a = TL.toStrict <$> upl a

-- find file in subdirs
findPaper :: FilePath -> FilePath -> IO [FilePath]
findPaper top match = find always (fileName ~~? match) top

-- random useful thing
third :: (a, b, c) -> c
third (_,_,c) = c

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
changeWholePath :: FilePath -> FilePath
changeWholePath fp =  uncurry combine . fixName $ splitFileName fp
    where fixName (x,y) = (x,fixZ y)

-- ugly, but works, kinda?
fixZ :: String -> String
fixZ ('p':'a':'g':'e':'-':xs) = "page-" <> killZeroes xs
fixZ n                        = n

killZeroes :: [Char] -> [Char]
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
  (Nothing, Nothing, Just erra, pid) <- createProcess (proc "git" ["add", "-A"]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  resultAdd <- decodeUtf8 <$> BS.hGetContents erra
  _ <- waitForProcess pid
  (Nothing, Nothing, Just errc, pidc) <- createProcess (proc "git" ["commit", "-m", "added understanding"]) { cwd = Just fp, std_in = NoStream, std_out = NoStream, std_err = CreatePipe, close_fds = True}
  exitCode <- waitForProcess pidc
  resultCommit <- decodeUtf8 <$> BS.hGetContents errc
  return (exitCode, resultAdd <> resultCommit)

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
unstupid :: Text -> Text
unstupid = T.replace "/repos" "" . T.replace "api." ""

swizzle :: Text -> Text -> Text -> Text
swizzle username oauth = T.replace "https://" ("https://" <> username <> ":" <> oauth <> "@")

-- any comment that says "this should be doing X" means "write a new function that does X"
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
checkGitConfig :: String -> IO ExitCode
checkGitConfig value = do -- value should be either "user.name" or "user.email"
  (Nothing, Nothing, Nothing, pidc) <- createProcess (proc "git" ["config",value]) { cwd = Just ".", std_in = NoStream, std_out = NoStream, std_err = NoStream, close_fds = True}
  exitCode <- waitForProcess pidc
  return exitCode

-- looks like names imported from Lib.Github are not automatically exported? who knew?!

createDR = createDataRepo
pnRepo = pairNameRepo
decodeSR :: BSL.ByteString -> Maybe Wrapper
decodeSR = decode

-- search result code here, probably needs to be in its own module
-- The Aeson code below is from an hour long pairing session with @maxhallinan
-- thanks for coming up with this idea and convincing me it would work!
emptyWrapper :: Wrapper
emptyWrapper = Wrapper "" (Message 0 [])
data Wrapper = Wrapper { status :: T.Text
                 , message      :: Message
                 } deriving (Show, Eq, Ord)
instance FromJSON Wrapper where
    parseJSON (Object v) = Wrapper <$> v .: "status" <*> v .: "message"
    parseJSON _          = error "we didn't do this yet!"

data Message = Message {
      total_result :: Integer
    , items        :: [SResult]
    } deriving (Show, Ord, Eq)
instance FromJSON Message where
    parseJSON (Object v) = Message <$> v .: "total-results" <*> v .: "items"
    parseJSON _          = error "no"

data SResult = SResult {
      doi     :: T.Text
    , page    :: Maybe T.Text
    , stitle  :: [T.Text]
    , volume  :: Maybe T.Text
    , authors :: Maybe [Author]
    , pubDate :: Maybe PubDate
    } deriving (Show, Eq, Ord)
instance FromJSON SResult where
    parseJSON (Object v) = SResult
                           <$> v .: "DOI"
                           <*> v .:? "page"
                           <*> v .: "title"
                           <*> v .:? "volume"
                           <*> v .:? "author"
                           <*> v .:? "published-print"
    parseJSON _          = error "bad SResult"

data Author = Author {
      given  :: Maybe T.Text
    , family :: T.Text
    } deriving (Show, Eq, Ord)
instance FromJSON Author where
    parseJSON (Object v) = Author
                           <$> v .:? "given"
                           <*> v .: "family"
    parseJSON _          = error "bad author"

newtype PubDate = PubDate [[Int]] deriving (Eq, Ord, Show)
instance FromJSON PubDate where
    parseJSON (Object v) = PubDate <$> v .: "date-parts"
    parseJSON _          = error "bad parseJSON for PubDate"

converter :: SResult -> Paper
converter s = Paper (doi s) (mkAuthors $ authors s) (mkPubDate $ pubDate s) (T.unwords $ stitle s) []

mkPubDate ::Maybe PubDate -> Day
mkPubDate mbpd = minimum $ buildparts <$> maybe [[1000,01,01]] (\(PubDate x) -> x) mbpd
    where buildparts ps = greg $ take 3 (ps <> repeat 0)
          greg [y,m,d] = fromGregorian (toInteger y) m d
          greg a       = error ("fromGregorian got bad input" <> show a)

mkAuthors :: Maybe [Author] -> T.Text
mkAuthors mbAs = T.unwords $ intersperse "," $ mkOneAuthor <$> fromMaybe [] mbAs

mkOneAuthor :: Author -> T.Text
mkOneAuthor a = fromMaybe "" (given a) <> " " <> family a

-- add a bunch of search results to the existing state
addFoundPapers :: FLMState -> [Paper] -> FLMState
addFoundPapers fs [] = fs
addFoundPapers fs (p:ps) = M.insertWith (flip const) (uid p) p (addFoundPapers fs ps)

-- sanity
sanitizePaper :: Paper -> Paper
sanitizePaper p = p {uid = T.toLower (uid p)}
