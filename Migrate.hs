module Main where

import           Control.Monad
import           Data.Aeson (decodeStrict)
import qualified Data.ByteString       as BS
import           Data.List             (isPrefixOf)
import           Data.Maybe
import           Lib
import           System.Directory
import           System.FilePath       ((</>))
import           System.FilePath.Find  (always, fileName, find, (~~?))

main :: IO ()
main = do
  fld <- (</> ".fermatslastmargin/localuser") <$> getHomeDirectory
  pp <- find always (fileName ~~? "paper.json") fld
  papers <- catMaybes <$> mapM (fmap decodeStrict . BS.readFile) pp
  mapM_ removeDirectoryRecursive =<< filterM doesDirectoryExist =<< (map (fld </>) . filter (not . isPrefixOf ".") <$> listDirectory fld)
  mapM_ (writePaper fld) papers
