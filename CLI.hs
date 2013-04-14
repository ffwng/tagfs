module CLI (
	Command(..),
	tagfsCLI,
	execParser
) where

import TagFS.Tag

import Options.Applicative
import Options.Applicative.Builder.Internal (Mod, OptionFields)

data Command =
	Mount [String] |
	AddFiles [FilePath] |
	RemoveFiles [FilePath] |
	Tag Tag [FilePath] |
	TagFile FilePath [Tag]

arguments' :: Parser [String]
arguments' = many $ argument' Just (metavar "ARGS")

args :: String -> Parser [String]
args n = arguments1 Just (metavar n)

mount :: Parser Command
mount = Mount <$> arguments'

addFiles :: Parser Command
addFiles = AddFiles <$> args "FILES"

removeFiles :: Parser Command
removeFiles = RemoveFiles <$> args "FILES"

maybeReader :: String -> (String -> Maybe a) -> Mod OptionFields a
maybeReader e f = reader (maybe (Left $ ErrorMsg e) Right . f)

tag :: Parser Command
tag = Tag <$> nullOption (maybeReader "Error: invalid tag." parseTag
	<> short 't' <> long "tag" <> metavar "TAG") <*> args "FILES"

tagFile :: Parser Command
tagFile = TagFile <$> strOption (short 'f' <> long "file" <> metavar "FILE")
	<*> arguments parseTag (metavar "TAGS")

tagfsCommand :: Parser Command
tagfsCommand = subparser
	(c "mount" mount "Mounts TagFS. All additional args are forwarded to fuse."
	<> c "add" addFiles "Adds files."
	<> c "remove" removeFiles "Removes files."
	<> c "tag" tag "Tags all given files with the given tag."
	<> c "tag-file" tagFile "Tags the given file with the given tags."
	) where
		c n p d = command n (info p (progDesc d))

tagfsCLI :: ParserInfo Command
tagfsCLI = info (helper <*> tagfsCommand)
	(fullDesc <> header "TagFS - A tagging file system")
