module RouteTest where

import Route hiding (Route)
import qualified Route as R

data Dir = Dir1 | Dir2 | Dir3 deriving Show

type Route a = R.Route String Dir a

dir1 :: Route Int
dir1 = do
	matchBranch (Just Dir1) "dir1"
	choice [dir2, dir3]

dir2 :: Route Int
dir2 = do
	matchLeaf "file2"
	return 2

dir3 :: Route Int
dir3 = do
	matchBranch Nothing "dir3"
	matchBranch (Just Dir3) "dir3a"
	choice [dir1, matchLeaf "file3" >> return 3]
