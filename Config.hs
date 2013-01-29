module Config where

import TagFS (TagSet)
import qualified Data.Map as M
import Data.Functor
import Safe

data Config = Config
	{ tagSet :: TagSet
	, mapping :: M.Map FilePath FilePath
	}
	deriving (Eq, Show, Read)

readConfig :: FilePath -> IO (Maybe Config)
readConfig f = readMay <$> readFile f

writeConfig :: FilePath -> Config -> IO ()
writeConfig f c = writeFile f $ show c
