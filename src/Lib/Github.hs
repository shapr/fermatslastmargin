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

{-
interface for this module is:
pass in the github username,
all friends are listed and checked for matching public repos named flmdata
return a list of tuples of (friend name, https url) for fetching each friend's flmdata directory
-}

-- | returns IO [(username, https url to flmdata)]
findRepos :: Name User -> String -> IO [(T.Text, T.Text)]
findRepos localusername oauthToken = do
  let auth = GitHub.OAuth . fromString $ oauthToken
  possibleUsers <- GitHub.executeRequest auth $ GitHub.usersFollowedByR localusername GitHub.FetchAll
  let friends = case possibleUsers of
                  Left e  -> error $ show e
                  Right u -> u
      userNames = fromUserName . simpleUserLogin <$> F.toList friends
  print $ "user is following " <> show (length friends) <> " friends."
  res <- sequence $ flmRepo <$> userNames
  pure $ pairNameRepo <$> rights res
      where pairNameRepo r = (untagName . simpleOwnerLogin $ repoOwner r, getUrl $ repoUrl r)

flmRepo = flip Repos.repository (mkName ([] :: [Repos.Repo]) "flmdata")

findRepos' :: T.Text -> T.Text -> IO [(T.Text,T.Text)]
findRepos' username token =
    findRepos (mkName ([] :: [User]) username) (T.unpack token)
