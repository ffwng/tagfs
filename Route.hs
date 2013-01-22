{-# LANGUAGE DeriveFunctor, TupleSections #-}
module Route where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Function

data Segment s t a =
	  Match t s a
	| Capture t (s -> Maybe a)
	| Choice [a]
	| NoRoute
	deriving (Functor)

type Route s t = Free (Segment s t)

{-routeToEnd :: Eq s => t -> Route s t a -> [s] -> Maybe (t, Route s t a)
routeToEnd t = flip (go t) where
	go :: Eq s => t -> [s] -> Route s t a -> Maybe (t, Route s t a)
	go t [] r = Just (t, r)
	go _ (x:xs) (Free (Match t s a)) | s == x = go t xs a
	go _ (x:xs) (Free (Capture t f)) = go t xs =<< f x
	go t xs (Free (Choice as)) = msum $ map (go t xs) as
	go _ _ _ = Nothing-}

route :: Eq s => t -> Route s t a -> [s] -> Maybe (Either t a)
route t r s = get t s r where
	get _ [] (Pure a) = Just $ Right a
	get t [] _ = Just $ Left t
	get t xs (Free (Choice as)) = msum $ map (get t xs) as
	get _ (x:xs) (Free (Match t s a)) | s == x = get t xs a
	get _ (x:xs) (Free (Capture t f)) = get t xs =<< f x
	get _ _ _ = Nothing

getBranch :: Eq s => Route s t a -> [s] -> Maybe (Either [(s, Either t a)] a)
getBranch r s = get s r where
	get [] (Pure a) = Just $ Right a
	get (x:xs) (Free (Match _ s a)) | s == x = get xs a
	get [] (Free (Match t s a)) = Just $ Left [(s, getPure t a)]
	get (x:xs) (Free (Capture _ f)) = get xs =<< f x
	get [] (Free (Capture _ _)) = Just $ Left []
	get xs (Free (Choice as)) = interpret' . catMaybes $ map (get xs) as
	get _ _ = Nothing

	interpret' [] = Nothing
	interpret' xs = Just $ interpret [] xs
	
	interpret acc [] = Left $ nubBy ((==) `on` fst) acc
	interpret acc ((Left x):xs) = interpret (x ++ acc) xs
	interpret _ ((Right a):_) = Right a

	getPure _ (Pure a) = Right a
	getPure t _ = Left t

match :: t -> s -> Route s t ()
match t s = liftF (Match t s ())

capture :: t -> (s -> Maybe a) -> Route s t a
capture t f = liftF (Capture t f)

choice :: [Route s t a] -> Route s t a
choice rs = join $ liftF (Choice rs)

noRoute :: Route s t a
noRoute = liftF NoRoute
