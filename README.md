# CSG: constructive solid geometry library

[![Travis CI build status](https://travis-ci.org/dzhus/csg.svg)](https://travis-ci.org/dzhus/csg)
[![Hackage](https://img.shields.io/hackage/v/csg.svg)](https://hackage.haskell.org/package/csg)
[![Hackage deps](https://img.shields.io/hackage-deps/v/csg.svg)](http://packdeps.haskellers.com/feed?needle=csg)

csg is a [constructive solid geometry][csg-wiki] library with support
for ray casting.

Please consult the [Hackage page for csg][hackage-doc] for full
documentation.

## csg-raycaster

This package also provides `csg-raycaster` tool, which is a simple
interactive GUI for the ray casting algorithm.

## Comparison to other libraries

csg library performs no surface interpolation when doing ray casting.
Instead, we only solve ray-surface intersection equation numerically.

There're other Haskell libraries for CSG:

- [implicit][]: uses marching cubes for ray casting (linear surface
  interpolation)

- [mecha][]: has no support for ray casting

[csg-wiki]: https://en.wikipedia.org/wiki/Constructive_solid_geometry
[hackage-doc]: http://hackage.haskell.org/package/csg/docs/Data-CSG.html
[implicit]: https://hackage.haskell.org/package/implicit
[mecha]: https://hackage.haskell.org/package/mecha
