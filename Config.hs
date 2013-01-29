module Config where

import TagFS (TagSet)
import qualified TagSet as T
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

addFile :: (FilePath, FilePath) -> Config -> Config
addFile (n,p) c = Config (T.addFile n $ tagSet c) (M.insert n p $ mapping c)

removeFile :: FilePath -> Config -> Config
removeFile n c = Config (T.removeFile n $ tagSet c) (M.delete n $ mapping c)
