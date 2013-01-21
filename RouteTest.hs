module RouteTest where

import Route hiding (Route)
import qualified Route as R

data Dir = Dir1 | Dir2 deriving Show

type Route a = R.Route String Dir a

dir1 :: Route Int
dir1 = do
	match "dir1"
	choice [dir2, dir3]

dir2 :: Route Int
dir2 = do
	match "dir2"
	return 2

dir3 :: Route Int
dir3 = do
	choice [dir1, return 3]
