module Main where


{-- semantics:

readdir - filter
link/sysmlink - tag assignment
unlink - tag deletion
mkdir - new tag
read - read

--}


type Tag = String


tagfsReadDir :: Tags -> FilePath -> [File]
tagfsReadDir tags (_:path) = query tags (parsePath path)

tagfsLink :: Tags -> FilePath -> FilePath -> Tags
tagfsLink tags (_:src) (_:dst) = addTags tags src (parsePath dst)

tagfsUnlink :: Tags -> FilePath -> FilePath -> Tags
tagfsUnlink tags (_:src) (_:dst) = removeTags tags src (parsePath dst)

