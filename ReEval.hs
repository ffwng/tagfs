{-# LANGUAGE ExistentialQuantification #-}
module ReEval where

import Data.IORef
import System.IO.Unsafe
import Control.Applicative
import Control.Monad

data Unshared a = forall b. Unshared (b -> a) b

unsharedValue :: Unshared a -> a
unsharedValue (Unshared f x) = f x

data ReEval a = ReEval {
	calculation :: Unshared a,
	currentValue :: IORef a
}

newReEval :: (b -> a) -> b -> ReEval a
newReEval f x = unsafePerformIO $ do
	let c = Unshared f x
	ref <- newIORef (unsharedValue c)
	return $ ReEval c ref

readReEval :: ReEval a -> a
readReEval r = unsafePerformIO $ readIORef (currentValue r)

resetReEval :: ReEval a -> IO ()
resetReEval r = writeIORef (currentValue r) (unsharedValue (calculation r))

instance Functor ReEval where
	fmap f (ReEval (Unshared f' x) v) = ReEval (Unshared (f . f') x) $
		unsafePerformIO $ readIORef v >>= newIORef . f

instance Monad ReEval where
	return = newReEval id
	v >>= f = fmap (readReEval . f) v

instance Applicative ReEval where
	pure = return
	(<*>) = ap
