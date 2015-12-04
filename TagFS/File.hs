{-# LANGUAGE DeriveFunctor #-}
module TagFS.File (
	File(..),
	FSTree(..),
	makeFSTrees
) where

import Prelude hiding (filter)
import qualified Prelude as P
import Data.List hiding (filter)
import Data.Function
import Data.Ord
import Data.Maybe
import Control.Arrow

-- | A file of the real file system.
newtype File = File { getPath :: [FilePath] }
	deriving (Eq, Ord, Show, Read)

data FSTree a = Leaf String a | Branch String [FSTree a] deriving (Eq, Show, Functor)

filter :: (a -> Bool) -> FSTree a -> Maybe (FSTree a)
filter f l@(Leaf _ a) = if f a then Just l else Nothing
filter f (Branch s l) = case mapMaybe (filter f) l of
	[] -> Nothing
	xs -> Just $ Branch s xs

groupHeadBy :: Eq b => (a -> (b, c)) -> [a] -> [(b, [c])]
groupHeadBy f = map (fst.head &&& map snd) . groupBy ((==) `on` fst) . map f

makeFSTrees :: [File] -> [FSTree File]
makeFSTrees fs = go . map (getPath &&& id) $ sortBy (comparing getPath) fs where
	go l = uncurry helper =<< groupHeadBy (\(p,f) -> (head p, (tail p, f))) l
	helper _ [] = []
	helper x pf = case span (\(a,_) -> null a) pf of
		(lf, bf) -> let leafs = map (Leaf x .  snd) lf in
			if null bf then leafs else Branch x (go bf) : leafs
