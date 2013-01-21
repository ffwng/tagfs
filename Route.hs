module Route (
	Route,
	matchBranch,
	matchLeaf,
	capture,
	choice,
	noRoute,
	runRoute,
	runRawRoute
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative
import Control.Arrow

import Data.Monoid
import Data.Maybe

data RouteStatus s t = RouteStatus
	{ consumed :: Bool
	, expecting :: Maybe [(s, Maybe (Maybe t))]
	}
	deriving (Show)

type Route s t a = StateT [s] (MaybeT (State (RouteStatus s t))) a

consume :: Route s t ()
consume = setConsumed True

setConsumed :: Bool -> Route s t ()
setConsumed b = lift . lift $ modify (\s -> s { consumed = b })

expect :: [(s, Maybe (Maybe t))] -> Route s t ()
expect xs = setExpecting (Just xs)

clearExpect :: Route s t ()
clearExpect = setExpecting Nothing

setExpecting :: Maybe [(s, Maybe (Maybe t))] -> Route s t ()
setExpecting x = lift . lift $ modify (\s -> s { expecting = x })

nextMaybe :: Route s t (Maybe s)
nextMaybe = do
	xs <- get
	case xs of
		p:ps -> do
			put ps
			return (Just p)
		_ -> noRoute

next :: Route s t s
next = maybe noRoute return =<< nextMaybe

match :: Eq s => Maybe (Maybe t) -> s -> Route s t ()
match t s = do
	expect [(s, t)]
	s' <- next
	clearExpect
	if s == s' then consume >> return ()
	else noRoute

matchBranch :: Eq s => Maybe t -> s -> Route s t ()
matchBranch t = match (Just t)

matchLeaf :: Eq s => s -> Route s t ()
matchLeaf = match Nothing

capture :: (s -> Maybe a) -> Route s t a
capture f = do
	expect []
	s <- next
	clearExpect
	case f s of
		Just a -> consume >> return a
		_ -> noRoute

captureBool :: (s -> Bool) -> Route s t s
captureBool f = capture (\s -> if f s then Just s else Nothing)

noRoute :: Route s t a
noRoute = mzero

concatMaybe :: [[a]] -> Maybe [a]
concatMaybe [] = Nothing
concatMaybe xs = Just $ concat xs

choice :: Eq s => [Route s t a] -> Route s t a
choice rs = do
	s <- get
	let res = map (`runRawRoute` s) rs
	-- gather expected from all routes, tag and value from first route
	let expected = concatMaybe . catMaybes $ map (expecting . snd) res
	setExpecting expected
	let pred (r, status) = isJust r || consumed status
	let succs = filter pred res
	case succs of
		(r, status):_ -> do
			setConsumed (consumed status)
			case r of
				Just (a, s') -> do
					put s'
					return a
				_ -> noRoute
		_ -> noRoute

runRawRoute :: Route s t a -> [s] -> (Maybe (a, [s]), RouteStatus s t)
runRawRoute r s = runIdentity . (`runStateT` (RouteStatus False Nothing)) . runMaybeT
	$ runStateT r s

runRoute :: Route s t a -> [s] -> (Maybe a, Maybe [(s, Maybe (Maybe t))])
runRoute r s = f $ runRawRoute r s where
	f (res, status) = (g res, expecting status)
	g (Just (a, x)) | null x = Just a
	g _ = Nothing
