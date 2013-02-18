module Config where

import TagFS (TagSet, Tag, File(..))
import qualified TagSet as T
import qualified Data.Map as M
import Control.Monad
import Data.Functor
import Control.Exception
import System.IO

data Config = Config
	{ tagSet :: TagSet
	, mapping :: M.Map File FilePath
	}
	deriving (Eq, Show, Read)

emptyConfig :: Config
emptyConfig = Config T.emptyTagSet M.empty

readConfig :: FilePath -> IO (Maybe Config)
readConfig f = either foo Just
	<$> try (withFile f ReadMode $ hGetContents >=> bar)
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
addFile (n,p) c = Config (T.addFile f $ tagSet c) (M.insert f p $ mapping c)
	where f = File n

removeFile :: FilePath -> Config -> Config
removeFile n c = Config (T.removeFile f $ tagSet c) (M.delete f $ mapping c)
	where f = File n

tagFile :: Tag -> FilePath -> Config -> Config
tagFile t f c = Config (T.addTag t (File f) $ tagSet c) (mapping c)
