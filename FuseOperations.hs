{-# LANGUAGE TupleSections #-}
module FuseOperations where

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Fuse hiding (RegularFile)
import System.IO
import Data.IORef
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import System.Posix.Files
import System.Posix.Types
import System.FilePath

import TagFS
import TagSet hiding (TagSet)
import Stat

data Status = Status
	{ getTagSet :: TagSet
--	, getRoute :: Route Entry
	, getFileMapping :: Map FilePath FilePath
	}

getRoute :: Status -> Route Entry
getRoute = buildBaseRoute . getTagSet

getRealPath :: Status -> FilePath -> FilePath
getRealPath s p = fromMaybe "/dev/null" $ M.lookup p (getFileMapping s)

newStatus :: TagSet -> Map FilePath FilePath -> Status
newStatus = Status

updateStatus :: Status -> TagSet -> Status
updateStatus s ts = s { getTagSet = ts }


-- helper functions

returnLeft :: Monad m => a -> m (Either a b)
returnLeft = return . Left

returnRight :: Monad m => b -> m (Either a b)
returnRight = return . Right

getEntryStat :: Status -> FuseContext -> Entry -> IO FileStat
getEntryStat s _ (RegularFile name) = realFileStat $ getRealPath s name
getEntryStat _ ctx (TagFile ts _ _) = return $ fileStat ctx (tagFileContentLength ts)

getDirStat :: Status -> FuseContext -> Dir -> IO FileStat
getDirStat _ ctx _ = return $ dirStat ctx

parseTag :: String -> Maybe Tag
parseTag s = case span (/= tagSep) s of
	(n, []) | notNull n -> Just (Simple n)
	(n, _:v) | notNull n && notNull v -> Just (Extended n v)
	_ -> Nothing
	where
		notNull = not . null

formatTag :: Tag -> String
formatTag (Simple n) = n
formatTag (Extended n v) = n ++ tagSep:v

tagFileContent :: [Tag] -> ByteString
tagFileContent = B.pack . unlines . map formatTag

tagFileContentLength :: [Tag] -> Int
tagFileContentLength = B.length . tagFileContent

parseTags :: ByteString -> [Tag]
parseTags = mapMaybe parseTag . lines . B.unpack

forFile :: Route Entry -> String -> IO a -> (Dir -> IO a) -> (Entry -> IO a)
	-> IO a
forFile r p noent nofile f = do
	let r' = route r p
	case r' of
		Nothing -> noent
		Just (Left dir) -> nofile dir
		Just (Right e) -> f e

forFile' :: Route Entry -> [String] -> IO a -> (Dir -> IO a)
	-> (Entry -> IO a) -> IO a
forFile' r p noent nofile f = do
	let r' = route' r p
	case r' of
		Nothing -> noent
		Just (Left dir) -> nofile dir
		Just (Right e) -> f e

forDir :: Route Entry -> String -> IO a -> (Entry -> IO a)
	-> (Dir -> IO a) -> IO a
forDir r p noent nodir f = do
	let r' = route r p
	case r' of
		Nothing -> noent
		Just (Right e) -> nodir e
		Just (Left dir) -> f dir

forDir' :: Route Entry -> [String] -> IO a -> (Entry -> IO a)
	-> (Dir -> IO a) -> IO a
forDir' r p noent nodir f = do
	let r' = route' r p
	case r' of
		Nothing -> noent
		Just (Right e) -> nodir e
		Just (Left dir) -> f dir

-- fuse operations

getFileStat :: IORef Status -> FilePath -> IO (Either Errno FileStat)
getFileStat ref p = do
	ctx <- getFuseContext
	status <- readIORef ref
	let r = getRoute status
	forFile r p (returnLeft eNOENT)
		(\d -> Right <$> getDirStat status ctx d)
		(\d -> Right <$> getEntryStat status ctx d)

-- directories

defaultStats :: FuseContext -> [(String, FileStat)]
defaultStats ctx = [(".", dirStat ctx), ("..", dirStat ctx)]

openDirectory :: IORef Status -> FilePath -> IO Errno
openDirectory _ _ = return eOK

readDirectory :: IORef Status -> FilePath
	-> IO (Either Errno [(FilePath, FileStat)])
readDirectory ref p = do
	ctx <- getFuseContext
	status <- readIORef ref
	let r = getRoute status
	let buildEntries entries = do
		stats <- mapM (makeStat status ctx) entries
		returnRight $ defaultStats ctx ++ stats
	maybe (returnLeft eNOENT) (maybe (returnLeft eNOTDIR) buildEntries) $ routeDir r p
	where
		makeStat _ c (n, Left _) = return (n, dirStat c)
		makeStat s c (n, Right e) = (n,) <$> getEntryStat s c e

softInit :: [a] -> [a]
softInit [] = []
softInit x = init x

createDirectory :: IORef Status -> FilePath -> FileMode -> IO Errno
createDirectory ref p _ = do
	let seg = split p
	let tag = parseTag (last seg)
	status <- readIORef ref
	let r = getRoute status
	let r' = route' r seg
	if isJust r'
	then return eEXIST
	else do
		let r'' = route' r (softInit seg)
		case (,) <$> tag <*> r'' of
			Just (Simple v, Left (ExtendedBaseDir name))
				-> newTag (Extended name v) status
			Just (_, Left (ExtendedBaseDir _)) -> return ePERM
			Just (tag', Left _) -> newTag tag' status
			_ -> return eINVAL
	where
		newTag tag status = do
			let ts = getTagSet status
			let tsNew = createTag tag ts
			writeIORef ref (updateStatus status tsNew)
			return eOK

removeDirectory :: IORef Status -> FilePath -> IO Errno
removeDirectory ref p = do
	let seg = split p
	status <- readIORef ref
	let r = getRoute status
	let ts = getTagSet status
	forDir' r seg (return eNOENT) (\_ -> return eNOTDIR) $ \x -> case x of
		TagDir t -> do
			let tsNew = wipeTag t ts
			writeIORef ref (updateStatus status tsNew)
			return eOK
		ExtendedBaseDir name -> do
			let tags' = filter ((== name) . getName) (tags ts)
			let tsNew = foldr wipeTag ts tags'
			writeIORef ref (updateStatus status tsNew)
			return eOK
		_ -> return ePERM

-- files

tempFile :: IO Handle
tempFile = do
	(path, handle) <- openTempFile "/tmp" ""
	removeLink path
	return handle

toIOMode :: OpenMode -> OpenFileFlags -> IOMode
toIOMode ReadOnly _ = ReadMode
toIOMode WriteOnly (OpenFileFlags a _ _ _ _) = if a then AppendMode else WriteMode
toIOMode ReadWrite _ = ReadWriteMode

tagfsOpen ::  IORef Status -> FilePath -> OpenMode -> OpenFileFlags
	-> IO (Either Errno Handle)
tagfsOpen ref p mode flags = do
	status <- readIORef ref
	let r = getRoute status
	let iomode = toIOMode mode flags
	forFile r p (returnLeft eNOENT) (\_ -> returnLeft ePERM) $ \x -> case x of
		RegularFile name -> do
			h <- openFile (getRealPath status name) iomode
			returnRight h
		TagFile t _ _ -> do
			h <- tempFile
			when (iomode /= WriteMode) $ B.hPut h (tagFileContent t)
			when (iomode /= AppendMode) $ hSeek h AbsoluteSeek 0
			returnRight h

tagfsRead :: IORef Status -> FilePath -> Handle -> ByteCount -> FileOffset
	-> IO (Either Errno ByteString)
tagfsRead _ _ h count offset = do
	hSeek h AbsoluteSeek (toInteger offset)
	Right <$> B.hGet h (fromInteger $ toInteger count)

tagfsWrite :: IORef Status -> FilePath -> Handle -> ByteString -> FileOffset
	-> IO (Either Errno ByteCount)
tagfsWrite _ _ h content offset = do
	hSeek h AbsoluteSeek (toInteger offset)
	B.hPut h content
	returnRight . fromInteger . toInteger $ B.length content

tagfsRelease :: IORef Status -> FilePath -> Handle -> IO ()
tagfsRelease ref p h = do
	status <- readIORef ref
	let r = getRoute status
	forFile r p (return ()) (const $ return ()) $ \x -> case x of
		TagFile _ name _ -> do
			hSeek h AbsoluteSeek 0
			content <- B.hGetContents h
			let ts = getTagSet status
			let tsNew = setTags (parseTags content) name ts
			writeIORef ref (updateStatus status tsNew)
		_ -> return ()
	hClose h

tagfsSetFileSize :: IORef Status -> FilePath -> FileOffset -> IO Errno
tagfsSetFileSize ref p size = do
	status <- readIORef ref
	let r = getRoute status
	forFile r p (return eNOENT) (const $ return eINVAL) $ \x -> case x of
		RegularFile name
			-> setFileSize (getRealPath status name) size >> return eOK
		TagFile {} -> return eOK

tagfsCreateLink :: IORef Status -> FilePath -> FilePath -> IO Errno
tagfsCreateLink ref src dst = do
	status <- readIORef ref
	let r = getRoute status
	let dstpath = dropFileName dst
	forFile r src (return eNOENT) (const $ return eINVAL) $ \f -> case f of
		RegularFile name ->
			forDir r dstpath (return eNOENT) (const $ return eNOTDIR) $ \d -> case d of
				TagDir tag -> do
					let ts = getTagSet status
					let tsNew = addTag tag name ts
					writeIORef ref (updateStatus status tsNew)
					return eOK
				_ -> return eINVAL
		_ -> return eINVAL


-- filesystem

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats _ = returnRight FileSystemStats
	{ fsStatBlockSize = 510
	, fsStatBlockCount = 1
	, fsStatBlocksFree = 1
	, fsStatBlocksAvailable = 1
	, fsStatFileCount = 5 -- IS THIS CORRECT?
	, fsStatFilesFree = 10 -- WHAT IS THIS?
	, fsStatMaxNameLength = 255 -- SEEMS SMALL?
	}

fsOps :: IORef Status -> FuseOperations Handle
fsOps r = defaultFuseOps
	{ fuseGetFileStat = getFileStat r
	, fuseOpenDirectory = openDirectory r
	, fuseReadDirectory = readDirectory r
	, fuseCreateDirectory = createDirectory r
	, fuseRemoveDirectory = removeDirectory r
	, fuseOpen = tagfsOpen r
	, fuseRead = tagfsRead r
	, fuseWrite = tagfsWrite r
	, fuseRelease = tagfsRelease r
	, fuseSetFileSize = tagfsSetFileSize r
	, fuseCreateLink = tagfsCreateLink r
	, fuseGetFileSystemStats = getFileSystemStats
	}
