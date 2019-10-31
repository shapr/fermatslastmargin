{-# LANGUAGE OverloadedStrings #-}
module Lib.Github where

import           Data.Either             (rights)
import qualified Data.Foldable           as F
import qualified Data.Text               as T
import qualified GitHub
import           GitHub.Data.Definitions
import qualified GitHub.Endpoints.Repos  as Repos
import           GitHub.Endpoints.Users
import           GitHub.Internal.Prelude (fromString)
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
  res <- sequence $ flmRepo <$> userNames
  let result = pairNameRepo <$> rights res
  print $ "found valid friend repos " <> show result
  pure result

pairNameRepo r = (untagName . simpleOwnerLogin $ repoOwner r, getUrl $ repoUrl r)

flmRepo = flip Repos.repository (mkName ([] :: [Repos.Repo]) "flmdata")

findRepos' :: T.Text -> T.Text -> Manager -> IO [(T.Text,T.Text)]
findRepos' username token mgmt =
    findRepos (mkName ([] :: [User]) username) (T.unpack token) mgmt


createDataRepo oauthToken = do
  let auth = GitHub.OAuth . fromString $ oauthToken
  Repos.createRepo' auth flmdatarepo

-- empty flmdata repo for initial startup
flmdatarepo = NewRepo {
                newRepoName = mkName ([] :: [Repo]) "flmdata"
              , newRepoDescription = Just "Data repo for Fermat's Last Margin"
              , newRepoHomepage = Nothing
              , newRepoPrivate = Just False
              , newRepoHasIssues = Just False
              , newRepoHasWiki = Just False
              , newRepoAutoInit = Just True -- GitHub, please just setup something for me!
              }
