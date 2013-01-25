{-# LANGUAGE TupleSections #-}
module TagSet where

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

data Tag = Simple String | Extended String String deriving (Eq, Ord, Show)
data TagSet = TagSet (Set Tag) (Map FilePath (Set Tag))

getName :: Tag -> String
getName (Simple n) = n
getName (Extended n _) = n

getValue :: Tag -> String
getValue (Simple v) = v
getValue (Extended _ v) = v

emptyTagSet :: TagSet
emptyTagSet = TagSet S.empty M.empty

fromFiles :: [Tag] -> [(FilePath, [Tag])] -> TagSet
fromFiles tags = TagSet (S.fromList tags) . M.fromList . map (second S.fromList)

-- query functions

queryFiles :: (Set Tag -> Bool) -> TagSet -> [FilePath]
queryFiles f (TagSet _ ts) = M.keys $ M.filter f ts

tags :: TagSet -> [Tag]
tags (TagSet x _) = S.toList x

files :: TagSet -> [String]
files (TagSet _ ts) = M.keys ts

queryTags :: FilePath -> TagSet -> Maybe [Tag]
queryTags name (TagSet _ ts) = S.toList <$> M.lookup name ts


-- modifiers

addTag :: Tag -> FilePath -> TagSet -> TagSet
addTag t name (TagSet x ts) = TagSet (S.insert t x) $ M.adjust (S.insert t) name ts

removeTag :: Tag -> FilePath -> TagSet -> TagSet
removeTag t name (TagSet x ts) = TagSet x $ M.adjust (S.delete t) name ts

setTags :: [Tag] -> FilePath -> TagSet -> TagSet
setTags t name (TagSet x ts) = TagSet (S.union x tags) $ M.adjust (const tags) name ts
	where tags = S.fromList t

createTag :: Tag -> TagSet -> TagSet
createTag t (TagSet x ts) = TagSet (S.insert t x) ts

wipeTag :: Tag -> TagSet -> TagSet
wipeTag t (TagSet x ts) = TagSet (S.delete t x) $ M.map (S.delete t) ts


-- serialization

groupFirst :: Eq a => [(a,b)] -> [(a, [b])]
groupFirst = map f . groupBy ((==) `on` fst) where
	f l = (fst $ head l, map snd l)

getTagAssocs :: TagSet -> [(FilePath, Tag)]
getTagAssocs (TagSet _ m) = [(p,t) | (p, s) <- M.toList m, t <- S.toList s]

craftTagSet :: [Tag] -> [FilePath] -> [(FilePath, Tag)] -> TagSet
craftTagSet t f a = let start = TagSet (S.fromList t) (M.fromList $ map (,S.empty) f)
	in foldr (uncurry $ flip addTag) start a
