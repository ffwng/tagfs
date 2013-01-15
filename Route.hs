{-# LANGUAGE DeriveFunctor #-}
module Route where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.List

import Text.PrettyPrint

data Segment a = Match String a
	| Capture (String -> Maybe a)
	| Choice [a]
	| EOR a   -- ^ End Of Route (ha ha)
	| NoRoute
	deriving Functor

type Route = Free Segment

match :: String -> Route ()
match a = liftF (Match a ())

capture :: (String -> Maybe a) -> Route a
capture = liftF . Capture

captureBool :: (String -> Bool) -> Route String
captureBool f = capture (\a -> if f a then Just a else Nothing)

choice :: [Route a] -> Route a
choice = join . liftF . Choice

eor :: Route ()
eor = liftF (EOR ())

isEOR :: Route Bool
isEOR = choice [eor >> return True, return False]

noRoute :: Route a
noRoute = liftF NoRoute

-- Nothing -> route failed
-- Just Nothing -> route succeeded, but had not finished
-- Just (Just x) -> route had finished with x
runRoute :: Route a -> [String] -> Maybe (Maybe a)
runRoute r s = getPureOrEOR <$> routeToEnd r s

-- Nothing -> route failed
-- Just Nothing -> route succeeded, but leaved no rest segments
-- Just (Just e) -> route succeeded and leaved rest segments e
-- Note: a route may both finish and leave rest segments
-- (consider choice [eor >> return "finished", match "name" >> return "restsegment"]
getRestSegments :: Route a -> [String] -> Maybe (Maybe [(FilePath, Maybe a)])
getRestSegments r s = findEntries <$> routeToEnd r s

foldRoute :: Route a -> Route a
foldRoute (Free (Match s a)) = Free (Match s (foldRoute a))
foldRoute (Free (Choice as)) = foldChoice' as
foldRoute r = r

foldChoice' as = case map flatten $ groupBy sameMatch as of
	[] -> noRoute
	[x] -> x
	xs -> choice xs
	where
		sameMatch (Free (Match s1 _)) (Free (Match s2 _)) = s1 == s2
		sameMatch _ _ = False
		flatten []  = noRoute
		flatten [x] = foldRoute x
		flatten xs@(Free (Match p _) : _) = Free (Match p (foldChoice'
			[ next | (Free (Match _ next)) <- xs]))
		flatten _ = error "flatten: assertion failed"

routeToEnd :: Route a -> [String] -> Maybe (Route a)
routeToEnd = go where
	go a [] = Just a
	go (Free (Match s a)) (x:xs) | s == x = routeToEnd a xs
	go (Free (Capture f)) (x:xs) = f x >>= flip routeToEnd xs
	go (Free (Choice as)) xs = msum $ map (`routeToEnd` xs) as
	go _ _ = Nothing

getPureOrEOR :: Route a -> Maybe a
getPureOrEOR = go where
	go = getPureHelper findEOR
	findEOR (Free (EOR a)) = go a
	findEOR _ = Nothing

getPure :: Route a -> Maybe a
getPure = getPureHelper (const Nothing)

getPureHelper :: (Route a -> Maybe a) -> Route a -> Maybe a
getPureHelper f = go where
	go (Pure a) = Just a
	go (Free (Choice as)) = msum $ map go as
	go r = f r

findEntriesMap :: Route a -> Maybe (Map String (Maybe a))
findEntriesMap (Free (Match s a)) = Just $ M.singleton s (getPure a)
findEntriesMap (Free (Choice a)) = Just . M.unions . catMaybes $ map findEntriesMap a
findEntriesMap _ = Nothing

findEntries :: Route a -> Maybe [(String, Maybe a)]
findEntries r = M.toList <$> findEntriesMap r

prettyRoute :: (Show a) => Route a -> Doc
prettyRoute (Pure a) = text "return" <+> text (show a)
prettyRoute (Free (Match p next)) = text "match"
	<+> doubleQuotes (text p) $+$ prettyRoute next
prettyRoute (Free (Capture f)) = text "capture <func>"
	<> text (show (fmap prettyRoute (f "")))
prettyRoute (Free (Choice cs)) = text "choice"
	<+> (cat $ map (\r -> text "do" <+> (nest 1 $ prettyRoute r)) cs)
prettyRoute (Free (EOR a)) = text "eor" $+$ prettyRoute a
prettyRoute (Free NoRoute) = text "no route"
