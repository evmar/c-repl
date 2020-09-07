    c-repl: a C read-eval-print loop.
    Copyright (C) 2008 Evan Martin <martine@danga.com>
    
 
 ---
 
 **Licenced under** :  
 
 [![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Many programming languages come with a REPL (read-eval-print loop),
which allows you to type in code line by line and see what it does. This
is quite useful for prototyping, experimentation, and debugging code.

Other programming languages, and especially C, use a "compile-run"
model, and don't provide a REPL. Let's fix that.

## Dependencies
- GHC 6.8
- gcc
- gccxml and hexpat
  (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hexpat)
- gdb and hgdbmi
  (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hgdbmi)
- readline

Debian/Ubuntu users on recent releases can do something like:
```bash
  sudo apt-get install ghc6 gccxml libghc6-parsec-dev libghc6-mtl-dev \
                       libghc6-hunit-dev
```
hexpat, hgdbmi, and readline can be fetched and installed from Hackage
via the above URLs or via cabal-get, and they depend on
  sudo apt-get install gdb libexpat1-dev c2hs libreadline-dev

If you get an error from c2hs like this:  

    /usr/include/bits/pthreadtypes.h:99: (column 6) [FATAL]
    >>> Syntax error!
    The symbol `;' does not fit here.  
    
then you unfortunately need a newer c2hs; the one in Ubuntu Hardy is
at least recent enough.

## Building 

Almost list a normal cabal-managed app:
```bash
  cabal configure
  cabal install
```
but with one major exception, you must also run this at the end.
```bash
  cabal copy
```

Why is this extra step necessary?  Read Setup.lhs and tell me what I've
done wrong; I've probably spent as much time trying to figure out Cabal
as I have writing the actual app.  I'd love to apply a patch from someone
smarter than me.

## Usage
Type normal lines of C code and hit enter.  
Trailing semicolons are optional.  
All variable and function declarations are implicitly global,  
but can be initialized as if they were locals.  

```bash
  > int x = 3
  > printf("at %p, %d\n", &x, x)
  at 0xb7f4a550, 3
  > FILE* f = fopen("README", "r")
```

Bring in more headers by writing #include statements.  Library functions
that are in scope should be tab-completable at the prompt.  

```bash
  > #include <stdlib.h>
  > op<TAB>
  open            open_memstream  openat64        
  open64          openat          
  > open
```

## How it works

The approach is surprisingly simple: for each line of code you enter, we
compile a shared object in the background. If the compilation succeeds,
the object is loaded into a child process via dlopen().  Parsing of C
#includes uses gccxml.  (Unfortunately, I can't figure out how to use
gccxml to parse the user's input, and due to the complexity of parsing C
the input parser is currently hacky and heuristic.)

## Debugging

c-repl currently can take one flag, "-v", which causes it to output the
internal code that it's generating.  
Please include this output with bug
reports.

## Credit

The original idea is due to Satoru Takabayashi (http://0xcc.net),  
who was responsible for a prototype implementation and advice on the
original version.
  
  
---  


**Use :set tw=72 to set the wrap width;**  
