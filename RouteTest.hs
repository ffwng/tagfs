module RouteTest where

import Route hiding (Route)
import qualified Route as R
import qualified Data.Set as S

data Dir = Dir1 | Dir2 | Dir3 deriving Show

type Route a = R.Route String (Maybe Dir) a

dir1 :: Route Int
dir1 = do
	match (Just Dir1) "dir1"
	choice [dir2, dirS]

dirS :: Route Int
dirS = do
	s <- matchSet (Just Dir3) (S.fromList ["15", "21", "137"])
	return $ read s

dir2 :: Route Int
dir2 = do
	match Nothing "file2"
	return 2

dir3 :: Route Int
dir3 = do
	match Nothing "dir3"
	match (Just Dir3) "dir3a"
	choice [dir1, match Nothing "file3" >> return 3]

dirEvil :: Route Int
dirEvil = choice [match Nothing "a" >> dir2, match Nothing "a" >> dir3]
