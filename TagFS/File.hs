{-# LANGUAGE DeriveFunctor #-}
module TagFS.File (
	File(..),
	FSTree(..),
	makeFSTree
) where

import Prelude hiding (filter)
import qualified Prelude as P
import Data.List hiding (filter)
import Data.Function

-- | A file of the real file system.
newtype File = File { getPath :: [FilePath] }
	deriving (Eq, Ord, Show, Read)

data FSTree a = Empty | Leaf String a | Branch String [FSTree a] deriving (Eq, Show, Functor)

filter :: (a -> Bool) -> FSTree a -> FSTree a
filter _ Empty = Empty
filter f l@(Leaf _ a) = if f a then l else Empty
filter f (Branch s l) = case map (filter f) l of
	[] -> Empty
	xs -> Branch s xs

makeFSTree :: [File] -> FSTree File
makeFSTree fs = Branch "" . map helper $ groupBy ((==) `on` head . getPath) fs where
	helper [] = Empty
	helper xs@(x:_) = let s = head (getPath x) in Branch s $ undefined

