# 1 Billion Row Challenge (Haskell)

This repo is a solution to the 1brc problem. It is written is Haskell2010 and exists mainly as a de-rust project.

`input-source` is a git submodule for the problem and some solutions.

`haskell` is my code. It requires `hashable` and `unsorted-containers` as dependencies.

It can be run like so

``` shell
cd haskell
tail --lines=+3 ../input-source/data/weather_stations.csv | cabal run > output.csv
```

Timing and profiling are system dependant. A common problem is that the external depedencies are not built with profiling info.
