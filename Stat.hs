module Stat where

import System.Fuse
import System.Posix.Files

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat {
	statEntryType = Directory
	, statFileMode = foldr1 unionFileModes
		[ ownerReadMode
		, ownerExecuteMode
		, groupReadMode
		, groupExecuteMode
		, otherReadMode
		, otherExecuteMode
		]
		, statLinkCount = 2
		, statFileOwner = fuseCtxUserID ctx
		, statFileGroup = fuseCtxGroupID ctx
		, statSpecialDeviceID = 0
		, statFileSize = 4096
		, statBlocks = 1
		, statAccessTime = 0
		, statModificationTime = 0
		, statStatusChangeTime = 0
	}

fileStat :: Integral a => FuseContext -> a -> FileStat
fileStat ctx size = FileStat {
	statEntryType = RegularFile
	, statFileMode = foldr1 unionFileModes
		[ ownerReadMode
		, groupReadMode
		, otherReadMode
		, ownerWriteMode
		]
	, statLinkCount = 1
	, statFileOwner = fuseCtxUserID ctx
	, statFileGroup = fuseCtxGroupID ctx
	, statSpecialDeviceID = 0
	, statFileSize = fromIntegral size
	, statBlocks = 1
	, statAccessTime = 0
	, statModificationTime = 0
	, statStatusChangeTime = 0
	}

linkStat :: Integral a => FuseContext -> a -> FileStat
linkStat ctx size = FileStat {
	statEntryType = SymbolicLink
	, statFileMode = accessModes
	, statLinkCount = 1
	, statFileOwner = fuseCtxUserID ctx
	, statFileGroup = fuseCtxGroupID ctx
	, statSpecialDeviceID = 0
	, statFileSize = fromIntegral size
	, statBlocks = 1
	, statAccessTime = 0
	, statModificationTime = 0
	, statStatusChangeTime = 0
	}

realFileStat :: FilePath -> IO FileStat
realFileStat uri = do
	status <- getFileStatus uri
	return FileStat {
		statEntryType = RegularFile
		, statFileMode = fileMode status
		, statLinkCount = linkCount status
		, statFileOwner = fileOwner status
		, statFileGroup = fileGroup status
		, statSpecialDeviceID = specialDeviceID status
		, statFileSize = fileSize status
		, statBlocks = 1 -- This is WRONG. Change
		, statAccessTime= accessTime status
		, statModificationTime = modificationTime status
		, statStatusChangeTime = statusChangeTime status
		}


