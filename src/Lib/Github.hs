{-# LANGUAGE OverloadedStrings #-}
module Lib.Github where

import           Data.Char               (isAsciiLower, isAsciiUpper, isDigit)
import           Data.Either             (rights)
import qualified Data.Foldable           as F
import qualified Data.Text               as T
import qualified GitHub
import           GitHub.Data.Definitions
import qualified GitHub.Endpoints.Repos  as Repos
import           GitHub.Endpoints.Users
import           GitHub.Internal.Prelude (fromString)
import qualified GitHub.Request          as GR
import           Network.HTTP.Client     (Manager)

{-
interface for this module is:
pass in the github username,
all friends are listed and checked for matching public repos named flmdata
return a list of tuples of (friend name, https url) for fetching each friend's flmdata directory
-}

-- | returns IO [(username, https url to flmdata)]
findRepos :: Name User -> String -> Manager -> IO [(T.Text, T.Text)]
findRepos localusername oauthToken mgmt = do
  let auth = GitHub.OAuth . fromString $ oauthToken
  possibleUsers <- GitHub.executeRequestWithMgr mgmt auth $ GitHub.usersFollowedByR localusername GitHub.FetchAll
  let friends = case possibleUsers of
                  Left e  -> error $ show e
                  Right u -> u
      userNames = fromUserName . simpleUserLogin <$> F.toList friends
  print $ "user is following " <> show (length friends) <> " friends."
  res <- sequence $ flmRepo auth <$> userNames
  let result = pairNameRepo <$> rights res
  print $ "found valid friend repos " <> show result
  pure result

pairNameRepo :: Repo -> (T.Text, T.Text)
pairNameRepo r = (untagName . simpleOwnerLogin $ repoOwner r, getUrl $ repoUrl r)

flmRepo :: AuthMethod am => am -> Name Owner -> IO (Either Error Repo)
flmRepo auth = GR.github auth $ flip Repos.repositoryR (mkName ([] :: [Repos.Repo]) "flmdata")

findRepos' :: T.Text -> T.Text -> Manager -> IO [(T.Text,T.Text)]
findRepos' username token mgmt =
    findRepos (mkName ([] :: [User]) username) (T.unpack token) mgmt

createDataRepo :: [Char] -> IO (Either Error Repo)
createDataRepo oauthToken = do
  let auth = GitHub.OAuth . fromString $ oauthToken
  GR.github auth $ Repos.createRepoR flmdatarepo

-- empty flmdata repo for initial startup
flmdatarepo :: NewRepo
flmdatarepo = NewRepo {
                newRepoName = mkName ([] :: [Repo]) "flmdata"
              , newRepoAllowMergeCommit = Just True
              , newRepoAllowRebaseMerge = Just True
              , newRepoAllowSquashMerge = Just True
              , newRepoAutoInit = Just True -- GitHub, please just setup something for me!
              , newRepoDescription = Just "Data repo for Fermat's Last Margin"
              , newRepoGitignoreTemplate = Nothing
              , newRepoHasIssues = Just False
              , newRepoHasProjects = Just False
              , newRepoHasWiki = Just False
              , newRepoHomepage = Nothing
              , newRepoLicenseTemplate = Nothing -- this really needs to be a good documentation license
              , newRepoPrivate = Just False
              }

-- | Basic validation of Github authentication tokens. Returns `Just` the token
-- if it is valid, `Nothing` otherwise.
--
-- Based on
-- https://github.blog/changelog/2021-03-31-authentication-token-format-updates-are-generally-available/
validateAuthToken :: T.Text -> Maybe T.Text
validateAuthToken token
  | validPrefix && validChars = Just token
  | otherwise = Nothing
  where
  validChars = T.all isValidChar token
  isValidChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

  validPrefix = any (`T.isPrefixOf` token) prefixes
  prefixes = ["ghp_" , "gho_" , "ghu_" , "ghs_" , "ghr_"]
