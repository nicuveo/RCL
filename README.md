## *RCL*

A [Remember The Milk](https://www.rememberthemilk.com) *CLI* and *API kit*.

**This is a work in progress!**

It currently builds a *rcl* executable that fetches the raw json list
of incomplete tasks of the user, and a *RCL* Haskell *API kit*.


## Install

*RCL* is built with Stackage's [LTS Haskell 1.9](http://www.stackage.org/snapshot/lts-1.9).

``` bash
cd RCL
wget http://www.stackage.org/snapshot/lts-1.9/cabal.config
cabal update
cabal configure
cabal install
```


## Remarks

This product uses the [Remember The Milk](https://www.rememberthemilk.com) *API* but is not endorsed or
certified by [Remember The Milk](https://www.rememberthemilk.com).
