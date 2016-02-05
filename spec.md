# Command Line Interface

`spp` is actually two programs, `spp` and `spp --clean`.

In both cases, you provide a path to the directory to process as `spp --src path/to/dir`.

## Output and Backup Directories

You can optionally specify an output and backup directory.

### Backup form
```
spp --src <src>
spp --src <src> --bak <bak>
```

In this mode, `spp` copies files to a backup diirectory, then processes the files in place. By default, `spp` uses the backup form with backup directory `<src-directory>.bak`.

### Output form
```
spp --src <src> --out <out>
```

In this mode, `spp` copies files to the output directory and then processes them there.

## Start Of Directive statement
`spp` allows a start-of-directive-statement, much like CPP's `#`. This can be useful, e.g., if you are working in a syntax highlighting environment and want to use a comment character from your language to be the directive start. Do so by passing the option `--directive-start <start-of-line>`

## No Clean on Errors Statement

You can tell `spp` to be permissive of parse errors and not automatically run `--clean` if an error occurs. Do so by adding the option `--no-clean-on-errors`.


# Preprocessor directives

## Layout

The basic layout of a preprocessor directive is:

```
<optional-line>
<directive-start>preprocess
    <directives>
<rest-of-document>
```

The text to be preprocessed is:

```
<optional-line>
<rest-of-document>
```

The optional line is a good idea in general.

## The Replace Directive

A replace directive is a statement that eanbles the replacement of some regex with some regex replacement. This has the format:

```
replace <regex> -> <replacement>
```

where both regex and replacement are specified as [Haskell Strings](http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html) whose values are posix regexes and replacements where "\\1", etc. refer to substrings.

## The Execute and Pasthrough Directive

These directives both specify a program in the format

```
<exec | pass> <program>
```

The program specified will be executed with the additional argument of the path of the file. It is executed from the parent directory of the current file, and is fed the contents of the current file. In other words, it is as if this is performed:

```bash
working_dir=$PWD
cd $parent_dir
cat $filename | program $filename > $filename
cd $working_dir
```

The `> $filename` is included for only `pass`, not for `exec`.

## The DoWrite Directive

This directive takes the simple, literal form

```
writeout
```

This directive enables the following commands in the text:

```
write <path> <- ~~~<text>~~~
```

where path is a [Haskell String](http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html).

The entire directive will be deleted and `<text>` will be written to the given file, which is a path relative to the parent directory of the current file.

## The Include Directive

This directive is similar to `DoWrite` in the way that it takes the form of a single word:

```
include
```

It enables C Preprocessor style includes of the form

```
include <path>
```

This statement is replaced by the contents of the file at that path.
