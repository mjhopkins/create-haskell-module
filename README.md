# create-haskell-module

A simple command-line tool for adding a new module to an existing Haskell project. Because editor support is too slow and clunky.

```
Usage: new-module [options] FQN

Create a new empty Haskell module, creating directories as required.

Command options:
  -d, --dir directory        Directory in which to create module (defaults to src).
  -f, --force                Overwrite the file if it already exists.
```

### Example usage

```shell
new-hs-module Data.Foo.Bar
```

```shell
new-hs-module --test Data.Foo.BarTest
```
