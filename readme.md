[tower](https://tonyday567.github.com/tower) [![Build Status](https://travis-ci.org/tonyday567/tower.png)](https://travis-ci.org/tonyday567/tower)
===

See [rendered](https://tonyday567.github.io/tower/index.html) [Tower.lhs](./src/Tower.lhs) for commentary.

Tower.Vector is the start of vector representation using applicative (just because it worked neatly).


usage
---

rendered with:

~~~
stack build && pandoc -f markdown+lhs -i src/Tower.lhs -t html -o index.html --filter pandoc-include --mathjax
~~~
