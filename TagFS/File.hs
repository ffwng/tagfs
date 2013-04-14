module TagFS.File (
	File(..)
) where

-- | A file of the real file system.
newtype File = File { getPath :: FilePath }
	deriving (Eq, Ord, Show, Read)

