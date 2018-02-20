# CSG: constructive solid geometry library

[![Travis CI build status](https://travis-ci.org/dzhus/csg.svg)](https://travis-ci.org/dzhus/csg)
[![Hackage](https://img.shields.io/hackage/v/csg.svg?colorB=5e5184&style=flat)](https://hackage.haskell.org/package/csg)
[![Hackage deps](https://img.shields.io/hackage-deps/v/csg.svg)](http://packdeps.haskellers.com/feed?needle=csg)

CSG is a [constructive solid geometry][csg-wiki] library with support
for ray casting. CSG allows you to define a complex solid as a
composition of primitives. It also provides functions to perform ray
casting (find an intersection of a ray and the defined solid) or test
whether a point belongs to the solid (for Monte Carlo volume
calculation).

Please consult the [Hackage page for csg][hackage-doc]
for full documentation.

See [alternatives](#alternatives) too.

## csg-raycaster

The package also includes `csg-raycaster` executable, which is a
simple interactive GUI for the ray casting algorithm.

`csg-raycaster` takes a geometry defintion file as input. See
[`cube.geo`](examples/cube.geo):

```
solid box = orthobrick (-150, -150, -150; 150, 150, 150);

solid rounded = sphere (0, 0, 0; 200);

solid roundedbox = rounded and box;

solid cylinder1 = cylinder (-160, 0, 0; 160, 0, 0; 100);
solid cylinder2 = cylinder (0, -160, 0; 0, 160, 0; 100);
solid cylinder3 = cylinder (0, 0, -160; 0, 0, 160; 100);

solid cross = cylinder1 or cylinder2 or cylinder3;

solid cutout = not cross;

solid top = roundedbox and cutout;

tlo top;
```

Please consult the [Hackage page for Data.CSG.Parser][parser-doc] for
full format specification.

`csg-raycaster` may be run as

```
csg-raycaster cube.geo
```

Run as `csg-raycaster --help` to see all options.

![csg-raycaster demo](csg-raycaster.gif)

When run without a file argument, `csg-raycaster` will try to display
an arbitrary CSG solid.

In the GUI window the following controls are supported:

| Input                     | Function                             |
|---------------------------|--------------------------------------|
| Left mouse button + drag  | Rotate                               |
| Right mouse button + drag | Pan                                  |
| Mouse wheel up            | Zoom in                              |
| Mouse wheel down          | Zoom out                             |
| `r`                       | Reset zoom level and camera position |

## Alternatives

csg library performs no surface interpolation when doing ray casting.
Instead, we only solve ray-surface intersection equation numerically.

There're other Haskell libraries for CSG:

- [implicit][]:

    - Offers a much richer operation set

    - Uses function representation for CSG solids

    - If `implicit` had ray-casting support in early 2012 then I
      probably wouldn't write `csg`.

- [mecha][]:

    - Only provides types and functions to define solids and export
      definitions to external formats

    - No support for ray casting

[csg-wiki]: https://en.wikipedia.org/wiki/Constructive_solid_geometry
[hackage-doc]: http://hackage.haskell.org/package/csg/docs/Data-CSG.html
[implicit]: https://hackage.haskell.org/package/implicit
[mecha]: https://hackage.haskell.org/package/mecha
[parser-doc]: http://hackage.haskell.org/package/csg/docs/Data-CSG-Parser.html
