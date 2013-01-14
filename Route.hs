{-# LANGUAGE DeriveFunctor #-}
module Route where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Maybe

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

routeToEnd :: Route a -> [String] -> Maybe (Route a)
routeToEnd = go where
	go a [] = Just a
	go (Free (Match s a)) (x:xs) | s == x = routeToEnd a xs
	go (Free (Capture f)) (x:xs) = case f x of
		Nothing -> Nothing
		Just a -> routeToEnd a xs
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

findEntries :: Route a -> Maybe [(String, Maybe a)]
findEntries (Free (Match s a)) = Just [(s, getPure a)]
findEntries (Free (Choice a)) = Just . concat . catMaybes $ map findEntries a
findEntries _ = Nothing

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
