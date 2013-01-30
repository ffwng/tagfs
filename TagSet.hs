{-# LANGUAGE TupleSections #-}
module TagSet (
	TagSet,
	emptyTagSet,
	fromFiles, queryTags,
	queryFiles, queryFilesSet,
	tags, files,
	addTag, removeTag, setTags,
	addFile, removeFile,
	createTag, wipeTag,
	getTagAssocs, craftTagSet
) where

import Prelude hiding (any)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative
import Control.Arrow (second)

{- |
	A multi-bimap between files and tags. It is possible to have both
	tags not associated to any file and files not associated to any tags,
	as well as multiple tags per file and multiple files per tag.
-}
data TagSet f t = TagSet (Set t) (Map f (Set t))
	deriving (Eq, Show, Read)

-- | Creates an empty 'TagSet'.
emptyTagSet :: TagSet f t
emptyTagSet = TagSet S.empty M.empty

-- | Creates a 'TagSet' from a list of tags and a @(f, [t])@ association
--   list.
fromFiles :: (Ord t, Ord f) => [t] -> [(f, [t])] -> TagSet f t
fromFiles ts = TagSet (S.fromList ts) . M.fromList . map (second S.fromList)

-- query functions

-- | Gets a list of file names, whose tags (given as @'Set' t@) statisfy the
--   given predicate.
queryFiles :: (Set t -> Bool) -> TagSet f t -> [f]
queryFiles f (TagSet _ ts) = M.keys $ M.filter f ts

queryFilesSet :: (Set t -> Bool) -> TagSet f t -> Set f
queryFilesSet f (TagSet _ ts) = M.keysSet $ M.filter f ts

-- | Gets a list of all tags in the 'TagSet'.
tags :: TagSet f t -> [t]
tags (TagSet x _) = S.toList x

-- | Gets a list of all files in the 'TagSet'. This is equivalent to
--   @queryFiles (const True)@.
files :: TagSet f t -> [f]
files (TagSet _ ts) = M.keys ts

-- | Gets a list of all tags for a given file. 'Nothing' is returned, if this file is
--   not contained in the 'TagSet'.
queryTags :: Ord f => f -> TagSet f t -> Maybe [t]
queryTags name (TagSet _ ts) = S.toList <$> M.lookup name ts


-- modifiers

-- | Assigns a t to a file in the 'TagSet'.
addTag :: (Ord t, Ord f) => t -> f -> TagSet f t -> TagSet f t
addTag t name (TagSet x ts) = TagSet (S.insert t x) $ M.adjust (S.insert t) name ts

-- | Removes a t from a file in the 'TagSet'. If this tag was not associated
--   to the file, the original 'TagSet' is returned.
--
--   Note: this will not remove the t from the 'TagSet' itself, even if there
--   are no files with this t left afterwards.
removeTag :: (Ord t, Ord f) => t -> f -> TagSet f t -> TagSet f t
removeTag t name (TagSet x ts) = TagSet x $ M.adjust (S.delete t) name ts

-- | Overwrites the ts of a file in the 'TagSet'.
setTags :: (Ord t, Ord f) => [t] -> f -> TagSet f t -> TagSet f t
setTags t name (TagSet x ts) = TagSet (x `S.union` tset) $ M.adjust (const tset) name ts
	where tset = S.fromList t

-- | Adds a t to the 'TagSet' without associating a file to it.
createTag :: Ord t => t -> TagSet f t -> TagSet f t
createTag t (TagSet x ts) = TagSet (S.insert t x) ts

-- | Completey removes a t from a 'TagSet' and all its files.
--   Afterwards, the tag is not returned by 'tags' or 'queryTags' anymore.
wipeTag :: Ord t => t -> TagSet f t -> TagSet f t
wipeTag t (TagSet x ts) = TagSet (S.delete t x) $ M.map (S.delete t) ts

-- | Adds a file. If the file was already present, it is reinserted with no tags.
addFile :: Ord f => f -> TagSet f t -> TagSet f t
addFile f (TagSet x ts) = TagSet x $ M.insert f S.empty ts

-- | Removes a file from the 'TagSet'. If the file was not present, the original
--   'TagSet' is returned.
removeFile :: Ord f => f -> TagSet f t -> TagSet f t
removeFile f (TagSet x ts) = TagSet x $ M.delete f ts

-- serialization

-- | Gets a tag association list for the 'TagSet'. Files with no tags or tags
--   with no files are not contained in the list. A tag with multiple files or
--   a file with multiple tags has multiple entries, one for each combination.
getTagAssocs :: TagSet f t-> [(f, t)]
getTagAssocs (TagSet _ m) = [(p,t) | (p, s) <- M.toList m, t <- S.toList s]

{- |
	Creates a 'TagSet' from a list of t, a list of files and an association list.
	
	The following property holds for any 'TagSet' @ts@:

@
ts == craftTagSet (tags ts) (files ts) (getTagAssocs ts)
@
-}
craftTagSet :: (Ord t, Ord f) => [t] -> [f] -> [(f, t)] -> TagSet f t
craftTagSet t f a = let start = TagSet (S.fromList t) (M.fromList $ map (,S.empty) f)
	in foldr (uncurry $ flip addTag) start a
