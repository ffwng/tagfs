module Config where

import TagFS (TagSet, Tag)
import qualified TagSet as T
import qualified Data.Map as M
import Data.Functor
import Control.Exception
import System.IO

data Config = Config
	{ tagSet :: TagSet
	, mapping :: M.Map FilePath FilePath
	}
	deriving (Eq, Show, Read)

emptyConfig :: Config
emptyConfig = Config T.emptyTagSet M.empty

readConfig :: FilePath -> IO (Maybe Config)
readConfig f = either foo Just
	<$> try (withFile f ReadMode $ \h -> bar =<< hGetContents h)
	where
		foo :: SomeException -> Maybe Config  -- resolves ambiguities of Exception
		foo _ = Nothing
		bar :: String -> IO Config
		bar s = do
			_ <- evaluate (length s)
			return (read s)

writeConfig :: FilePath -> Config -> IO ()
writeConfig f c = writeFile f $ show c

addFile :: (FilePath, FilePath) -> Config -> Config
addFile (n,p) c = Config (T.addFile n $ tagSet c) (M.insert n p $ mapping c)

removeFile :: FilePath -> Config -> Config
removeFile n c = Config (T.removeFile n $ tagSet c) (M.delete n $ mapping c)

tagFile :: Tag -> FilePath -> Config -> Config
tagFile t f c = Config (T.addTag t f $ tagSet c) (mapping c)
