# finbound
Internal boundary processing library for Fortran.

# Install
```
git clone https://github.com/Nkzono99/finbound.git
cd finbound
vim Makefile  # modify fortran compiler(FC) and etc.
make
```

# Linker flags
```
LFLAGS = -Lfinbound/build/lib/ -Ifinbound/build/include/ -lfinbound

Including dependencies (after installing futils in the same way):
LFLAGS = -Lfutils/build/lib/ -Ifutils/build/include/ -lfutils -Lfinbound/build/lib/ -Ifinbound/build/include/ -lfinbound
```

# Dependencies
futils: https://github.com/Nkzono99/futils

# Faster queries for many boundaries

`t_BoundaryBVH` is an optional bounding-volume hierarchy built from an existing
`t_BoundaryList`. It skips groups of finite shapes that cannot intersect a segment
or ray, then calls the original shape methods for the remaining candidates.
Existing constructors, types, and `BoundaryList` methods remain available.

```fortran
use finbound
! list has already been populated using its existing add_boundary API.
type(t_BoundaryList) :: list
type(t_BoundaryBVH) :: fast
type(t_CollisionRecord) :: collision
type(t_HitRecord) :: intersection
type(t_Ray) :: ray
double precision :: p1(3), p2(3)

! ... populate list and initialize p1, p2, ray ...
fast = new_BoundaryBVH(list)
collision = fast%check_collision(p1, p2)
intersection = fast%hit(ray)

! After changing positions, radii, vertices, or rotations:
call fast%rebuild()
! After adding/removing/replacing top-level list entries:
call fast%build(list)

call fast%destroy()
call list%destroy()
```

The index **borrows boundary references**: keep their targets alive while querying
it. `fast%destroy()` frees only the index; it never destroys the original shapes.
Copies of an index have independent tree storage and borrow the same shapes.
Priority and material changes are visible without rebuilding. Geometry changes
require an explicit rebuild, because existing shape fields remain publicly writable.

Sphere, cut sphere, cylinder, circle, annulus, rectangle, triangle, ellipsoid,
hyperboloid, and finite rotated/nested boundaries receive conservative bounds.
Infinite planes and unrecognized user-defined extensions use a linear fallback.
The selected result follows the same distance, priority, and insertion-order rules
as `BoundaryList`. `is_overlap` retains the existing linear overlap semantics.
As with a list, use the selected shape's normal or `intersection%n`, rather than
the aggregate's `normal`/`pnormal`.

The existing rotation API also reuses sine/cosine within each query. It does not
cache mutable shape fields, so direct edits to `rotation_rad`, `axis`, or `origin`
still take effect immediately.

See [benchmark results and reproduction instructions](benchmark/README.md).
