<link rel="stylesheet" href="https://tonyday567.github.io/other/lhs.css">
<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
[tower](https://tonyday567.github.com/tower) [![Build Status](https://travis-ci.org/tonyday567/tower.png)](https://travis-ci.org/tonyday567/tower)
==================================================================================================================================================

A heirarchy of classes for numbers and algebras that combine them: a
numeric tower.

Performance testing, notes and examples can be found in
[tower-dev](https://github.com/tonyday567/tower-dev).

The tower looks something like:

![](https://tonyday567.github.io/other/field-tower.svg)

``` {.sourceCode .literate .haskell}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
```

``` {.sourceCode .literate .haskell}
import Tower.Prelude
```

`Tower.Prelude` is a drop-in replacement for `Prelude`. Behind the
scenes, it wraps `Protolude`.

~~~
stack build --test --exec "pandoc -f markdown -i readme.md -t html -o index.html --filter pandoc-include --mathjax"
~~~

