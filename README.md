# GenericPreprocessor
A preprocessor that is designed for simplicity and extensiblity, written in Haskell

## Motivation

Sometimes, you want to use a preprocessor. Some believe that it is not a good practice, but, seriously, sometimes refactoring doesn't cut it.

Now, if you're working in a language like C, it is pretty simple to do something like

```C
#define <macro name>(<args>) <body>
```

but even in these languages, the preprocessor is often very limited in its scope. In other more dynamic languages, you can do something like

```python
preprocessed_code = process("""
<your code here>
""")
eval(preprocessed_code)
```

but here, syntax highlighting is gone. In some languages, such as Java, you have no access to a preprocessor at all.

Of course, you might easily be able to write your own simple one-time-use preprocessor that does something that you want to do. For example, in Java, you might want to convert expressions of the form `[a, b]` into `Arrays.asList(a, b)`.

So, you can write your basic preprocessor unit, and then use some command line magic to apply it to every file in a folder, moving it into another folder and then compiling the files in that folder.

OK, that's a completely ridiculous amount of work. Also, the overhead of storing all your work in `src-unpreprocessed` is a pain. So, you stick to writing the ugly original syntax.

But there is another way. Just have a generic program that simulates \*nix's `#!/path/to/program/to/run/this/with` ability. `spp` is this program.

## How `spp` works

`spp` works in two phases: processing and cleanup. In processing, the given source directory is copied into a backup folder. `spp` then goes through each file in the original folder, processes it, and dumps the result back in the file. In cleaning, the original directory is removed and the backups are restored.

## Command Line Interface

`spp` is actually two programs, `spp` and `spp --clean`.

In both cases, you provide a path to the directory to process as `spp --src path/to/dir`.

Additional options

 - you can provide a string to be used as a requirement for a start-of-directive-statement, much like CPP uses `#`. This can be useful, e.g., if you are working in a syntax highlighting environment and want to use a comment character from your language to be the directive start. Do so by adding the option `--directive-start "start of line"`
 - you can tell `spp` to be permissive of parse errors and not automatically run `--clean` if an error occurs. Do so by adding the option `--no-clean-on-errors`
