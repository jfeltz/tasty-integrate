tasty-integrate
---------------
tasty-integrate brings automated test-harness creation (such as what naturally occurs in
Eclipse) to the Haskell world.  This is a set of programs to automate the
otherwise manual editing needed to combine new tests
([QuickCheck](http://hackage.haskell.org/package/QuickCheck)) into 
test-groupings ([tasty](https://hackage.haskell.org/package/tasty) test framework).

Why
---
Inorder to do Continuous Integration in Haskell, previously one had to manually
edit test-suites following the creation of a new Modules (containing QuickCheck
properties). tasty-integrate removes this tedium.

tasty-integrate is principally:

 * <b>tasty-integrate</b>  : a program to automatically integrate test files into a target suite via set-union or set-mask
 * <b>ide-format</b> : a simple stream-parser to filter test harness errors into gcc-like errors for easy parsing and browsing by editors such as vim and emacs. 

tasty-integrate example
-----------------------

Given a set of Modules containing properties: 

    $ ls tests/Data/*
    QcIntegrated.hs  QcModulePath.hs  QcSuite.hs

And running tasty-integrate on these modules:

    $ tasty-integrate test-suites/Suite.hs tests/Data/Qc*.hs 
    (created) suite at: test-suites/Suite.hs
    with: 
      Data.QcIntegrated, 3 property(s)
      Data.QcModulePath, 2 property(s)
      Data.QcSuite, 1 property(s)

The suite is now created, and can be compiled as a cabal test-suite.  If a
filesystem change occurs, such as 2 of the modules no longer being necessary or
used, tasty-integrate can be ran as follows:

    $ tasty-integrate --masking test-suites/Suites.hs --masking test-suites/Suite.hs tests/Data/QcIntegrated.hs
    (changed) suite at: test-suites/Suite.hs
    with: 
     added: 
      none
     removed: 
       Data.QcModulePath, 2 property(s)
       Data.QcSuite, 1 property(s)
     modified: 
      none
     unmodified: 
       Data.QcIntegrated, 3 property(s)

ide-format example
-----------------------
Suites produced by tasty-integrate are formatted to resolve to the location of
the failing property/test. So piping the suite created by test-integrate through ide-format:

    $ ./build/dist/suite/suite | ide-format 
    tests/Data/QcSuite.hs|20 error | buf_isomorphism
       *** Failed! Falsifiable (after 1 test): 

For trivial parsing by editors such as vim and emacs.

Installation
------------
   
    git clone https://github.com/jfeltz/tasty-integrate.git
    cd tasty-integrate
    cabal configure 
    cabal build 
    cabal install 

Using tasty-integrate
---------------------

    $ ./dist/build/whatever/built-suite/built-suite | ide-format

tasty-integrate Behavior
------------------------

## TODO
* Support for Smallcheck and Hunit
* The end-game is to patch this into tasty or test-framework (via specific
console TestRunner), or possibly move this functionality into a
larger collaborative Haskell-in-Production foundation.
