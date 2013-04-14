module TagFS.Tag where

{- |
	A tag of a file. There are two possible forms of tags:

	* Simple tags: these tags have a name and act as simple
	  on/off tags. Either they are present or not for a given file. A file may not
	  have two simple tags of the same name.

	* Extended tags: these tags have a name and an associated 'String' value.
	  This way, further information can be provided, when these tags are present.
	  A file can have multiple extended tags with the same name but different values,
	  but no two extended tags of the same name and value.

	It is possible but highly discouraged to have a file with a simple and an
	extended tag of the same name.
-}
data Tag = Simple String | Extended String String
	deriving (Eq, Ord, Show, Read)

-- | Extracts the name of a 'Tag'.
getName :: Tag -> String
getName (Simple n) = n
getName (Extended n _) = n

-- | Extracts the value of a 'Tag'. For simple tags, the name is returned instead.
getValue :: Tag -> String
getValue (Simple v) = v
getValue (Extended _ v) = v

{- |
	Seperator used to seperate name and value of an extended tag's string representation.
	The current value is ':'.
-}
tagSep :: Char
tagSep = ':'

{- |
	Parses a tag from a string. The following formats are recognized:

	* /name/:/value/ → 'Extended' /name/ /value/
	
	* /name/ → 'Simple' /name/
-}
parseTag :: String -> Maybe Tag
parseTag s = case span (/= tagSep) s of
	(n, []) | notNull n -> Just (Simple n)
	(n, _:v) | notNull n && notNull v -> Just (Extended n v)
	_ -> Nothing
	where
		notNull = not . null

{- |
	Format a tag to 'String' representation. The following format is used:

	* 'Simple' /name/ → /name/
	
	* 'Extended' /name/ /value/ → /name/:/value/

	It holds for any 'Tag' /t/, that:

	> parseTag (formatTag t) == Just t
-}
formatTag :: Tag -> String
formatTag (Simple n) = n
formatTag (Extended n v) = n ++ tagSep:v
