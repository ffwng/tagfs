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

choice :: [Route a] -> Route a
choice = join . liftF . Choice

eor :: Route ()
eor = liftF (EOR ())

noRoute :: Route a
noRoute = liftF NoRoute

runRoute :: Route a -> [String] -> Maybe (Maybe a)
runRoute r s = getPure <$> routeToEnd r s

getRestSegments :: Route a -> [String] -> Maybe (Maybe [(FilePath, Maybe a)])
getRestSegments r s = findEntries <$> routeToEnd r s

routeToEnd :: Route a -> [String] -> Maybe (Route a)
routeToEnd r s = go r s where
	go a [] = Just a
	go (Free (Match s a)) (x:xs) | s == x = routeToEnd a xs
	go (Free (Capture f)) (x:xs) = case f x of
		Nothing -> Nothing
		Just a -> routeToEnd a xs
	go (Free (Choice as)) xs = msum $ map (`routeToEnd` xs) as
	go _ _ = Nothing


getPure :: Route a -> Maybe a
getPure (Pure a) = Just a
getPure (Free (EOR a)) = getPure a
getPure (Free (Choice as)) = msum $ map getPure as
getPure _ = Nothing

getPureWithoutEOR :: Route a -> Maybe a
getPureWithoutEOR (Pure a) = Just a
getPureWithoutEOR (Free (Choice as)) = msum $ map getPureWithoutEOR as
getPureWithoutEOR _ = Nothing

findEntries :: Route a -> Maybe [(String, Maybe a)]
findEntries (Free (Match s a)) = Just [(s, getPureWithoutEOR a)]
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
