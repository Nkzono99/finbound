# Performance checks

Build and run with Fortran Package Manager:

```sh
fpm run --example benchmark_collisions --compiler gfortran --profile release --flag '-O3'
fpm run --example benchmark_rotation --compiler gfortran --profile release --flag '-O3'
```

On KUDPC login nodes, execute these commands through `tssrun` or a batch job, e.g.:

```sh
tssrun -p gr20001g -t 0:05:00 --rsc p=1:t=1:c=1 \
  bash -lc 'cd /path/to/finbound && fpm run --example benchmark_collisions --compiler gfortran --profile release --flag "-O3"'
```

`benchmark_collisions` compares the original `BoundaryList` and `BoundaryBVH` in
the same executable, using identical deterministic inputs and exact checksum
comparisons. Each result is the median of three measurements of 10,000 queries,
after warming up both implementations and alternating measurement order. Tree
construction is measured separately. No fast-math flags are required.

Grid scenes contain separated spheres, with a mixture of hits and misses. The
overlap scene puts all spheres at the same location and makes every query hit
every sphere, so it exercises the case where the hierarchy cannot prune shapes.
Speedups depend on the geometry, query distribution, and compiler; the grid
results do not predict the speedup of every simulation.

`benchmark_rotation` runs 524,288 queries per measurement against one rotated
sphere and reports median times and checksums for segment, ray, and overlap
queries. Use the same compiler, flags, and compute node to compare revisions.

Correctness is checked separately by `fpm test`: the BVH test compares full
records (including normals, priority, and material) with the original list,
including mixed shapes, custom extensions, nested lists, ties, geometry edits,
empty trees, and tangent/parallel queries. The rotation test compares with the
original coordinate-transform formulas on all three axes and after direct edits
to public fields.

## Measured results

Measured on 2026-09-22 on KUDPC `ng0002`, AMD EPYC 7513, one allocated CPU core,
gfortran 8.5.0, FPM release profile plus `-O3`. CPU times below exclude compilation.
Both paths produced identical checksums. The rotation baseline is the working
tree after the bug fixes and before the performance changes.

| Rotation query (524,288 calls) | Before (s) | After (s) | Speedup |
| --- | ---: | ---: | ---: |
| Segment collision | 0.086752 | 0.050495 | 1.72× |
| Ray hit | 0.099582 | 0.065405 | 1.52× |
| Domain overlap | 0.259788 | 0.068108 | 3.81× |

| Scene | Query (10,000 calls) | BoundaryList (s) | BoundaryBVH (s) | Speedup |
| --- | --- | ---: | ---: | ---: |
| 16 separated spheres | Segment | 0.003925 | 0.000691 | 5.68× |
| 16 separated spheres | Ray | 0.004238 | 0.000963 | 4.40× |
| 256 separated spheres | Segment | 0.062126 | 0.001267 | 49.03× |
| 256 separated spheres | Ray | 0.060050 | 0.001483 | 40.49× |
| 4,096 separated spheres | Segment | 0.998262 | 0.002047 | 487.67× |
| 4,096 separated spheres | Ray | 0.957934 | 0.002241 | 427.46× |
| 256 coincident spheres, all queries hit | Segment | 0.072292 | 0.061725 | 1.17× |
| 256 coincident spheres, all queries hit | Ray | 0.342216 | 0.213851 | 1.60× |

Index construction took 0.000029 s, 0.000164 s, and 0.004128 s for the three grid
sizes, and 0.000088 s for the overlapping scene. Reuse the index across queries
to amortize construction; constructing one for every query defeats its purpose.

The debug and optimized gfortran suites passed 156,167 BVH comparison checks,
7,924 rotation compatibility checks, the earlier 192 regression checks, and all
pre-existing tests. Bounds checks and invalid/zero/overflow floating-point traps
were enabled. Public signatures and existing derived-type definitions were also
compared against the pre-optimization sources.

An additional nvfortran 23.9 build encountered the compiler's internal
`memsym_of_ast:unexp.ast` error. The pre-optimization baseline also fails with
that internal compiler error; runtime validation here is therefore limited to
gfortran.
