# CSG: constructive solid geometry library

[![Travis CI build status](https://travis-ci.org/dzhus/csg.svg)](https://travis-ci.org/dzhus/csg)
[![Hackage](https://img.shields.io/hackage/v/csg.svg)](https://hackage.haskell.org/package/csg)
[![Hackage deps](https://img.shields.io/hackage-deps/v/csg.svg)](http://packdeps.haskellers.com/feed?needle=csg)

csg is a [constructive solid geometry][csg-wiki] library with support
for ray casting.

Please consult the [Hackage page for csg][hackage-doc] for full
documentation.

## csg-raycaster

The package also includes `csg-raycaster` executable, which is a
simple interactive GUI for the ray casting algorithm.

`csg-raycaster` requires a geometry defintion file as input. See
[`cube.geo`](cube.geo):

```
solid box = orthobrick (-15, -15, -15; 15, 15, 15);

solid rounded = sphere (0, 0, 0; 20);

solid roundedbox = rounded and box;

solid cylinder1 = cylinder (-16, 0, 0; 16, 0, 0; 10);
solid cylinder2 = cylinder (0, -16, 0; 0, 16, 0; 10);
solid cylinder3 = cylinder (0, 0, -16; 0, 0, 16; 10);

solid cross = cylinder1 or cylinder2 or cylinder3;

solid cutout = not cross;

solid top = roundedbox and cutout;

tlo top;
```

`csg-raycaster` may be run as

```
csg-raycaster cube.geo
```

![csg-raycaster demo](csg-raycaster.gif)

Available controls:

| Input                     | Function                             |
|---------------------------|--------------------------------------|
| Left mouse button + drag  | Rotate                               |
| Right mouse button + drag | Pan                                  |
| Mouse wheel up            | Zoom in                              |
| Mouse wheel down          | Zoom out                             |
| `r`                       | Reset zoom level and camera position |

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
