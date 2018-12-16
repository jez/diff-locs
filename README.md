# diff-locs

[![Build diff-locs Status](https://travis-ci.org/jez/diff-locs.svg?branch=master)](https://travis-ci.org/jez/diff-locs)

> List the filename:line pairs involved in a diff

`diff-locs` takes a text diff (like, the output of `git diff`) and lists the
filename + line number pairs involved. This is useful in conjunction with other
tools that operate on such pairs:

- [`multi-grep`] is a tool that searchs for a pattern in a specific set of lines
- [`multi-sed`] is a tool that runs sed over a these specific lines
- In Vim, pressing `gf` or `gF` will open the file under the cursor at the
  specific line

[`multi-grep`]: https://github.com/jez/multi-grep
[`multi-sed`]: https://github.com/jez/bin/blob/master/multi-sed

For example, given this `git show` diff output:

<details>
<summary>Click to expand diff output</summary>

```diff
❯ git show 3f1b8e9 -- app/Main.hs
commit 3f1b8e9705e414a80203bf642fd708fd77f28dfc
Author: Jake Zimmerman <zimmerman.jake@gmail.com>
Date:   15 minutes ago

    Show warning message if stdin is a tty

diff --git a/app/Main.hs b/app/Main.hs
index 16d01e4..b0ecbd9 100644
--- a/app/Main.hs
+++ b/app/Main.hs
@@ -1,12 +1,16 @@
+{-# LANGUAGE LambdaCase     #-}
 {-# LANGUAGE NamedFieldPuns #-}
 module Main where

-import qualified System.IO          as IO
+import           Control.Monad         (when)
+import qualified System.IO             as IO
+import           System.Posix.IO       (stdInput)
+import           System.Posix.Terminal (queryTerminal)

 import           DiffLocs.InputLoop
 import           DiffLocs.Options
 import           DiffLocs.Types
-import           Paths_diff_locs    (version)
+import           Paths_diff_locs       (version)

 main :: IO ()
 main = do
@@ -15,6 +19,10 @@ main = do
   fileIn <- case optionsInput of
     -- Leak the file handle because we're short lived anyways
     InputFromFile filename -> IO.openFile filename IO.ReadMode
-    InputFromStdin         -> return IO.stdin
+    InputFromStdin         -> do
+      isTTY <- queryTerminal stdInput
+      when isTTY $ do
+        IO.hPutStrLn IO.stderr "Warning: reading from stdin, which is a tty."
+      return IO.stdin

   run $ Config {configFileIn = fileIn, configWhichLines = optionsWhichLines}
```

</details>

<br>

... you can run `diff-locs` to show the added / modified lines:

```
❯ git show 3f1b8e9 -- app/Main.hs | diff-locs
app/Main.hs:1
app/Main.hs:5
app/Main.hs:6
app/Main.hs:7
app/Main.hs:8
app/Main.hs:13
app/Main.hs:22
app/Main.hs:23
app/Main.hs:24
app/Main.hs:25
app/Main.hs:26
```

or pass a flag and see all lines affected (both added and removed):

```
❯ git show 3f1b8e9 -- app/Main.hs | diff-locs --all
+app/Main.hs:1
-app/Main.hs:4
+app/Main.hs:5
+app/Main.hs:6
+app/Main.hs:7
+app/Main.hs:8
-app/Main.hs:9
+app/Main.hs:13
-app/Main.hs:18
+app/Main.hs:22
+app/Main.hs:23
+app/Main.hs:24
+app/Main.hs:25
+app/Main.hs:26
```


## Install

### macOS

```
brew install jez/formulae/diff-locs
```

### Linux

Check the [Releases] page for pre-built 64-bit binaries for Linux.

[Releases]: https://github.com/jez/diff-locs/releases

### From source

To build this project from source, use Haskell's [Stack].

[Stack]: https://docs.haskellstack.org/en/stable/

```
git clone https://github.com/jez/diff-locs.git

stack build && stack install
```

If you've packaged this software for your operating system, let me know and I
can link to it from these instructions.


## TODO

- support `diff -u` output
- Add cli tests
- Bump version to 0.9.0

## License

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://jez.io/MIT-LICENSE.txt)

