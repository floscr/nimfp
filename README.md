# nimfp 

This is a fork of the [vegansk/nimfp](https://github.com/vegansk/nimfp) package, see [Fork Changes](#fork-changes).


Nim functional programming library. Includes:

* Maybe type [src/fp/maybe.nim](src/fp/maybe.nim)
* List type [src/fp/list.nim](src/fp/list.nim)
* Either type [src/fp/either.nim](src/fp/either.nim)
* Map type [src/fp/map.nim](src/fp/map.nim)
* Stream type [src/fp/stream.nim](src/fp/stream.nim)
* Scala like _for comprehension_ and Haskell like _do notation_ support [src/fp/forcomp.nim](src/fp/forcomp.nim)

While there is no documentation, you can see examples in the [tests/fp](tests/fp) directory.

## Fork changes

- Removes future & concurrent modules as they're broken in nim 1.6
- Fixes some tests that were using some library that was not declared in the nimble file
- Renames Option module to Maybe
  This way there are no clashes with nim's standard lib option, which were annoying when using this library and led to compilation errors

This is not an official nimble package yet, so you can add it like this to your `nimble` file.

``` nim
requires "https://github.com/floscr/nimfp#master"
```
