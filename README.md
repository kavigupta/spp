# Simple Preprocessor (spp)
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

## A typical use case

Let's say you have some files:

```txt
{src}
    information.md
    _includes
        example.py
```

The file `information.md` contains the following content:

```markdown

# A practical example

Here is an example of some python code:

\```python
#!/usr/bin/python
def f(g):
    return g(h)

h = 2

print(f(f))

\```

See `example.py` for the code sample provided here.

```

and the file `example.py` contains the content

```python
#!/usr/bin/python
def f(g):
    return g(h)

h = 2

print(f(f))
```

The "Don't Repeat Yourself" principle is being completely violated here. The simple solution is to use `spp`.

```markdown
preprocess:
    include
# A practical example

Here is an example of some python code:

\```python
include: "_includes/example.py"
\```

See `example.py` for the code sample provided here.

```

If you were using the command `markdown do magic {src}` to process the markdown files, you can now use

```sh
spp --src {src}
markdown do magic {src}
spp --clean --src {src}
```

And everything should just work. If you are, for example, using a service like `jekyll` or something that gives a live preview, this should work the same way, except that the files in `{src}` would have been replaced by postprocessed equivalents that would be write-protected. In this case, you can use `spp --update --src {src}` to update your current folder. You can even run an automatic update process every 5 seconds, `update` should simply be a fairly fast noop if none of the files have been modified since the last update.
