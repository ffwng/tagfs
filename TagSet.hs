{-# LANGUAGE TupleSections #-}
module TagSet (
	TagSet,
	Tag(..),
	getName, getValue,
	emptyTagSet,
	fromFiles, queryTags,
	queryFiles,
	tags, files,
	addTag, removeTag, setTags,
	createTag, wipeTag,
	getTagAssocs, craftTagSet
) where

import Prelude hiding (any)
import System.IO (FilePath)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List hiding (any)
import Control.Applicative
import Control.Arrow (second)
import Data.Foldable (any)
import Data.Function
import Data.Ord

{- |
	A multi-bimap between files and tags. It is possible to have both
	tags not associated to any file and files not associated to any tags,
	as well as multiple tags per file and multiple files per tag.
-}
data TagSet = TagSet (Set Tag) (Map FilePath (Set Tag))

{- |
	A tag of a file. There are two possible forms of tags:

	* Simple tags: these tags have a name and act as simple
	  on/off tags. Either they are present or not for a given file. A file may not
	  have two simple tags of the same name.

	* Extended tags: these tags have a name and an associated 'String' value.
	  This way, further information can be provided, when these tags are present.
	  A file can have multiple extended tags with the same name but different values,
	  but no two extended tags of the same name and value.

	It is possible but highly discouraged to have a file with a simple and an
	extended tag of the same name.
-}
data Tag = Simple String | Extended String String deriving (Eq, Ord, Show)

-- | Extracts the name of a 'Tag'.
getName :: Tag -> String
getName (Simple n) = n
getName (Extended n _) = n

-- | Extracts the value of a 'Tag'. For simple tags, the name is returned instead.
getValue :: Tag -> String
getValue (Simple v) = v
getValue (Extended _ v) = v

-- | Creates an empty 'TagSet'.
emptyTagSet :: TagSet
emptyTagSet = TagSet S.empty M.empty

-- | Creates a 'TagSet' from a list of tags and a @('FilePath', ['Tag'])@ association
--   list.
fromFiles :: [Tag] -> [(FilePath, [Tag])] -> TagSet
fromFiles tags = TagSet (S.fromList tags) . M.fromList . map (second S.fromList)

-- query functions

-- | Gets a list of file names, whose tags (given as @'Set' 'Tag'@) statisfy the
--   given predicate.
queryFiles :: (Set Tag -> Bool) -> TagSet -> [FilePath]
queryFiles f (TagSet _ ts) = M.keys $ M.filter f ts

-- | Gets a list of all tags in the 'TagSet'.
tags :: TagSet -> [Tag]
tags (TagSet x _) = S.toList x

-- | Gets a list of all files in the 'TagSet'. This is equivalent to
--   @queryFiles (const True)@.
files :: TagSet -> [String]
files (TagSet _ ts) = M.keys ts

-- | Gets a list of all tags for a given file. 'Nothing' is returned, if this file is
--   not contained in the 'TagSet'.
queryTags :: FilePath -> TagSet -> Maybe [Tag]
queryTags name (TagSet _ ts) = S.toList <$> M.lookup name ts


-- modifiers

-- | Assigns a 'Tag' to a file in the 'TagSet'.
addTag :: Tag -> FilePath -> TagSet -> TagSet
addTag t name (TagSet x ts) = TagSet (S.insert t x) $ M.adjust (S.insert t) name ts

-- | Removes a 'Tag' from a file in the 'TagSet'. If this tag was not associated
--   to the file, the original 'TagSet' is returned.
--
--   Note: this will not remove the 'Tag' from the 'TagSet' itself, even if there
--   are no files with this 'Tag' left afterwards.
removeTag :: Tag -> FilePath -> TagSet -> TagSet
removeTag t name (TagSet x ts) = TagSet x $ M.adjust (S.delete t) name ts

-- | Overwrites the 'Tag's of a file in the 'TagSet'.
setTags :: [Tag] -> FilePath -> TagSet -> TagSet
setTags t name (TagSet x ts) = TagSet (S.union x tags) $ M.adjust (const tags) name ts
	where tags = S.fromList t

-- | Adds a 'Tag' to the 'TagSet' without associating a file to it.
createTag :: Tag -> TagSet -> TagSet
createTag t (TagSet x ts) = TagSet (S.insert t x) ts

-- | Completey removes a 'Tag' from a 'TagSet' and all its files.
--   Afterwards, the tag is not returned by 'tags' or 'queryTags' anymore.
wipeTag :: Tag -> TagSet -> TagSet
wipeTag t (TagSet x ts) = TagSet (S.delete t x) $ M.map (S.delete t) ts


-- serialization

groupFirst :: Eq a => [(a,b)] -> [(a, [b])]
groupFirst = map f . groupBy ((==) `on` fst) where
	f l = (fst $ head l, map snd l)

-- | Gets a tag association list for the 'TagSet'. Files with no tags or tags
--   with no files are not contained in the list. A tag with multiple files or
--   a file with multiple tags has multiple entries, one for each combination.
getTagAssocs :: TagSet -> [(FilePath, Tag)]
getTagAssocs (TagSet _ m) = [(p,t) | (p, s) <- M.toList m, t <- S.toList s]

{- |
	Creates a 'TagSet' from a list of 'Tag's, a list of files and an association list.
	
	The following property holds for any 'TagSet' @ts@:

@
ts == craftTagSet (tags ts) (files ts) (getTagAssocs ts)
@
-}
craftTagSet :: [Tag] -> [FilePath] -> [(FilePath, Tag)] -> TagSet
craftTagSet t f a = let start = TagSet (S.fromList t) (M.fromList $ map (,S.empty) f)
	in foldr (uncurry $ flip addTag) start a
