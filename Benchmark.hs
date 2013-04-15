module Main where

import System.CPUTime
import System.FilePath
import Data.Maybe

import TagFS
import TagSet hiding (TagSet)

benchmark :: String -> IO a -> IO ()
benchmark name io = do
	putStrLn $ "Starting " ++ name ++ "…"
	start <- getCPUTime
	_ <- io
	end <- getCPUTime
	let t = fromInteger (end - start) / (1000000000000 :: Double)
	putStrLn $ name ++ " done in " ++ show t ++ " seconds."


ts :: TagSet
ts = fromFiles [] (map (\x -> (File [show x], [])) [1..10000 :: Int])

entryInt :: Either Dir Entry -> Int
entryInt (Left _) = 13
entryInt (Right _) = 17

ls :: Route Entry -> FilePath -> IO ()
ls r p = do
	-- first get all dir entries
	let Just (Just entries) = routeDir r p
	-- stat every entry (as ls would do)
	let stated = map (\(f,_) -> fromJust $ route r (p </> f)) entries
	-- perform evaluation
	let res = sum (map entryInt stated)
	print res

main :: IO ()
main = do
	let r = buildBaseRoute ts
	benchmark "ls" $ ls r "/"
