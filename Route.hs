{-# LANGUAGE DeriveFunctor, TupleSections #-}
module Route where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Function
import Data.Ord

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

route :: Eq s => t -> Route s t a -> [s] -> Maybe (Either t (a, t))
route t r s = get t s r where
	get t [] (Pure a) = Just $ Right (a, t)
	get t [] _ = Just $ Left t
	get t xs (Free (Choice as)) = msum $ map (get t xs) as
	get _ (x:xs) (Free (Match t s a)) | s == x = get t xs a
	get _ (x:xs) (Free (Capture t f)) = get t xs =<< f x
	get _ _ _ = Nothing

-- nubs a sorted list
sortedNubBy :: (a -> a -> Bool) -> [a] -> [a]
sortedNubBy _ [] = []
sortedNubBy _ [x] = [x]
sortedNubBy f (a:b:xs) | f a b = sortedNubBy f (b:xs)
sortedNubBy f (a:b:xs) = a : sortedNubBy f (b:xs)

getBranch :: Ord s => t -> Route s t a -> [s] -> Maybe (Either [(s, Either t (a, t))] (a, t))
getBranch t r s = get t s r where
	get t [] (Pure a) = Just $ Right (a, t)
	get _ (x:xs) (Free (Match t s a)) | s == x = get t xs a
	get _ [] (Free (Match t s a)) = Just $ Left [(s, getPure t a)]
	get _ (x:xs) (Free (Capture t f)) = get t xs =<< f x
	get _ [] (Free (Capture _ _)) = Just $ Left []
	get t xs (Free (Choice as)) = interpret' . catMaybes $ map (get t xs) as
	get _ _ _ = Nothing

	interpret' [] = Nothing
	interpret' xs = Just $ interpret [] xs
	
	interpret acc [] = Left . sortedNubBy ((==) `on` fst) $ sortBy (comparing fst) acc
	interpret acc ((Left x):xs) = interpret (x ++ acc) xs
	interpret _ ((Right a):_) = Right a

	getPure t (Pure a) = Right (a, t)
	getPure t _ = Left t

match :: t -> s -> Route s t ()
match t s = liftF (Match t s ())

capture :: t -> (s -> Maybe a) -> Route s t a
capture t f = liftF (Capture t f)

choice :: [Route s t a] -> Route s t a
choice rs = join $ liftF (Choice rs)

noRoute :: Route s t a
noRoute = liftF NoRoute
