# TagFS

## About

**TagFS** is a FUSE filesystem written in Haskell (using HFuse as binding). It
manages a set of files and allows filtering and accessing them by *tags* instead
of directories.

The main difference between tags and directories is, that a file can have
arbitrary many tags assigned, as opposed to just one directory. In a
conventional file system, the same can be accomplished using symlinks or
hardlinks, however, **TagFS** provides nice filtering possibilities like “give
me all files with tag *holiday* and tag *europe*”, which is difficult using
regular file systems.

## Requirements

* `HFuse` (Haskell binding for FUSE)
* `cabal` and `GHC` for building
* Linux/Unix (unfortunately, some functions are POSIX-only). Any platform which
  supports the `unix` package is supported.

## Installation

Clone the repository, change to its directory and run:

    cabal configure
    cabal build
    cabal install

## Basic Usage

After installation, change to a directory, which should become the root of a
**TagFS** structure (e.g. your root image directory). Before you can do
anything, **TagFS** have to know about the files it should manage. To do this,
run

    tagfs add *file…*

This automatically creates a file *tagfs.conf* containing all files and their
tags. With the same command, files can be added later. At the moment it is not
possible to have different files with the same file name both managed. In such a
case, the last one wins.
Use `tagfs remove *filename…*` to remove already managed files. Note that this
command does not accept full file paths, but only bare filenames.

You can now assign a bunch of files to a tag with

    tagfs tag *tag* *file…*

Non-managed files will be ignored. Tagging can also be done in the mounted
**TagFS** view.

To finally mount the file system, create an empty directory and execute

    tagfs mount *directory*

You can unmount it later with

    fusermount -u *directory*

## Tags

Tags come in two flavors. There are simple tags like *holiday*. They have a name
and simply describe the existence of an aspect of the file. Extended tags on the
other hand can be tought of as “simple tags with value“ and spelled out
*name:value*, e.g. *location:berlin* or *person:mom*. In most aspects, an
extended tag *name:value* behaves identically to a simple tag *name.value* (the
colon is not allowed in names of single tags, as it identicates an extended
tag), but extended tags are handled slightly different in some situations (see
below).

## File System Structure

### Regular Files

A regular file (i.e. a real file managed by **TagFS**) is currently presented as
a symbolic link pointing to the real file. This may change in future in favor of
a simple virtual regular file.

### Tag Files

For each regular file *file* there exists a tag file *file.tags*. This file
contains all tags of *file*, one per line. Extended tags are displayed in colon
form. When writing to this file, the tags of *file* are updated accordingly.
Note that the order of the tags in the tag file is not determined and may be
different to the order, they are written to it.

Tag files do not occur in directory listings (i.e. are not returned by
`readdir`) to tidy up the listings. In future, they may be presented as
*.file.tags* instead to provide autocompletion.

### Tag Directories

Each tag is represented as directory. Simple tags will just have a directory
with their name, extended tags have a directory with their name containing a
subdirectory for each value of this extended tag.

These directories will contain all files of the base directory, which have the
respective tag. That is, `/holiday` will contain all files tagged with
*holiday*, `/holiday/europe` contains all files tagged with *holiday* **and**
*europe*, or all files tagged with *holiday:europe*, depending on whether
*holiday* is a simple or an extended tag. Note that in case of simple tags,
`/holiday/europe` is the same as `/europe/holiday`.

A directory for a tag is not available somewhere below itself. That means, paths
like `/holiday/europe/holiday` are not possible. This prevents infinite
directories.

Additionally, for each tag dir(s) *dir*, the directories *and/dir* and *or/dir*
are provided. *and/dir* is equivalent to *dir* and only provided for
consistency. *or/dir* will contain all current files and all files with the
given tag. For instance, *holiday/or/europe* will give all files tagged with
holiday **or** europe.

A directory *not/dir* for a tag dir *dir* negates the condition: instead of
requiring to posses the tag, this selects all files **not** having the tag.
Similarly to before, *and/not/dir* is equivalent to *not/dir* and *or/not/dir*
selects all current files and all files not having the tag of *dir*.

These operations always associate to the left, i.e. are applied to the current
directory and the current set of files.
That is, `/holiday/and/europe/or/asia` will give all files tagged with
( *holiday* and *europe* ) or *asia*. `holiday/or/asia/and/europe` on the other
hand will select all files tagged with ( *holiday* or *asia* ) and *europe*.

### Tag Directories with other conditions

Given the structure described above, not all conditions on tags can be
described, mostly due to the restriction, that operatores always associate to
the left. For instance, given tags *a*, *b*, *c* and *d*, it is not possible to
filter exactly that files, that are tagged ( *a* or *b* ) and ( *b* or *c* ).
However, another kind of directories exists, which allows arbitrary conditions,
given as directory name, to get applied. They are not contained within the
directory listing, though, as infinitely many of them exist.

To get all files statisfying a certain condition *cond*, use the directory named
*?cond*. Additionally, *and/?cond* (equivalent to *?cond*, *or/?cond*,
*not/?cond*, *and/not/?cond* (equivalent to *not/?cond*) and *or/not/?cond* can
be used. The condition is always applied to the current set of files.

Any tag *t* (written as *name* for simple tags, *name:value* for extended tags)
is a condition, which is true for a file *f* iff *f* is tagged with *t*. If a
name or value contains non-alphanumeric characters (also whitespace) or begins
with a digit, it needs to be enclosed in quotation marks (`"`). If the tag
does not exist, the condition is always false.

Conditions can be combined using the operators `&&` (and), `||` (or) and `!`
(not) to form more complex condition.  So `a && b` is true for *f*, iff *f* is
tagged with *a* and with *b*, `!a` is true for *f*, iff *f* is not tagged with
*a*. `!` has highest priority, `&&` has a higher priority then `||`. Parens can
be used to explicitely group terms.

The special condition `true` is always true, `false` is always
false.

Another kind of condition has the form *name op value*, where *name* is a name
of a tag (subject to the same rules described above), *op* is a relational
operator (= (or ==), <, <=, >, >=) and *value* is either a name or an integer
(to treat an integer like a string, enclose it in quotation marks). This
condition is true for a file *f*, iff *f* is tagged with an extended tag, whose
name is *name*, and *value* *op* *v* is true, where *v* is the value of this
extended tag. For strings, lexiographic comparisments are done, for integers,
*v* is converted to an integer first (if this fails, the condition is false).
Note: A condition *name=value* is equivalent to *name:value* (and only provided
for consistency).
Note: A file can be tagged with many extended tags with the same name. In that
case, according to the above definition, any such extended tag statisfying the
relation will make the condition true.
Note: *<* is not the opposite of *>=*. For instance, `name < a` and `name >= a`
are both false, if the file is not tagged with an extended tag named *name*.
They can also be both true, if the file is tagged with an extended tag named
*name* multiple times. The same applies to the other operators.

Whitespace outside of quotation marks is ignored, so `a&&b` and `a && b` are
equivalent. Condition can be commented using `/* */` (for a comment inside the
condition) or `//` (comment extends to the end of condition). These comments are
ignored.

## File System Operations

### Creating tags

Tags can be created with `mkdir`. To create a simple tag, do `mkdir *name*`, for
an extended tag you can either use `mkdir *name:value*` or `mkdir *value*` in
the base directory of the tag. The latter is only possible if there is at least
another extended tag with the same name, otherwise the base directory does not
exist.

### Tagging files

Files can be tagged with hardlinks. That is, `ln file tag` will tag *file* with
*tag*. Symlinks are not supported, because they do not need to be linked to an
actual file, which has to be the case here. The tag must exist.
Alternatively, the tag file can be used as described above.

### Removing tags

Tags can be removed with `rmdir *dir*`. This will remove this tag from all files and
then remove the tag itself.

### Filtering files

Files can be filtered using the tag directories described above.
