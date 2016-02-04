## Command Line Interface

`spp` is actually two programs, `spp` and `spp --clean`.

In both cases, you provide a path to the directory to process as `spp --src path/to/dir`.

Additional options

 - you can provide a string to be used as a requirement for a start-of-directive-statement, much like CPP uses `#`. This can be useful, e.g., if you are working in a syntax highlighting environment and want to use a comment character from your language to be the directive start. Do so by adding the option `--directive-start "start of line"`
 - you can tell `spp` to be permissive of parse errors and not automatically run `--clean` if an error occurs. Do so by adding the option `--no-clean-on-errors`


## Preprocessor directives

### Layout

The basic layout of a preprocessor directive is:

```
<optional-line>
<directive-start>preprocess
    <directives>
<rest-of-document>
```

### The Replace Directive

A replace directive is a statement that eanbles the replacement of some regex with some regex replacement. This has the format:

```
replace <regex> -> <replacement>
```

where both regex and replacement are specified as [Haskell Strings](http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html) whose values are posix regexes and replacements where "\\1", etc. refer to substrings.

### The Execute and Pasthrough Directive

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
