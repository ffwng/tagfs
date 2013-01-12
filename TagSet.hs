module TagSet where

import System.IO (FilePath)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Control.Applicative

type Tag = String
data TagSet = TagSet (Set Tag) (Map FilePath (Set Tag))


-- query functions

query :: Tag -> TagSet -> TagSet
query t (TagSet x ts) = TagSet x $ M.filter (S.member t) ts

tags :: TagSet -> [Tag]
tags (TagSet x _) = S.toList x

files :: TagSet -> [String]
files (TagSet _ ts) = M.keys ts

queryTags :: FilePath -> TagSet -> Maybe [Tag]
queryTags name (TagSet _ ts) = S.toList <$> M.lookup name ts


-- modifiers

addTag :: Tag -> FilePath -> TagSet -> TagSet
addTag t name (TagSet x ts) = TagSet (S.insert t x) $ M.adjust (S.insert t) name ts

removeTag :: Tag -> FilePath -> TagSet -> TagSet
removeTag t name (TagSet x ts) = TagSet x $ M.adjust (S.delete t) name ts

setTags :: [Tag] -> FilePath -> TagSet -> TagSet
setTags t name (TagSet x ts) = TagSet (S.union x tags) $ M.adjust (const tags) name ts
	where tags = S.fromList t

createTag :: Tag -> TagSet -> TagSet
createTag t (TagSet x ts) = TagSet (S.insert t x) ts

wipeTag :: Tag -> TagSet -> TagSet
wipeTag t (TagSet x ts) = TagSet (S.delete t x) $ M.map (S.delete t) ts
