module Config where

import TagFS (TagSet)
import qualified TagSet as T
import qualified Data.Map as M
import Data.Functor
import Control.Exception

data Config = Config
	{ tagSet :: TagSet
	, mapping :: M.Map FilePath FilePath
	}
	deriving (Eq, Show, Read)

emptyConfig :: Config
emptyConfig = Config T.emptyTagSet M.empty

readConfig :: FilePath -> IO (Maybe Config)
readConfig f = (Just . read <$> readFile f) `catch` foo where
	foo :: SomeException -> IO (Maybe Config)  -- fixes ambiguous exception tipe
	foo _ = return Nothing

writeConfig :: FilePath -> Config -> IO ()
writeConfig f c = writeFile f $ show c

addFile :: (FilePath, FilePath) -> Config -> Config
addFile (n,p) c = Config (T.addFile n $ tagSet c) (M.insert n p $ mapping c)

removeFile :: FilePath -> Config -> Config
removeFile n c = Config (T.removeFile n $ tagSet c) (M.delete n $ mapping c)
