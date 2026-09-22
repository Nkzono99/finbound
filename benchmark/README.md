# Performance checks

Build and run with Fortran Package Manager:

```sh
fpm run --example benchmark_collisions --compiler gfortran --profile release --flag '-O3'
fpm run --example benchmark_rotation --compiler gfortran --profile release --flag '-O3'
fpm run --example benchmark_shapes --compiler gfortran --profile release --flag '-O3'
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

`benchmark_shapes` measures existing scalar `check_collision` and `hit` calls
against individual spheres, cut spheres, cylinders, triangles, and rectangles.
It uses 1,048,576 calls per measurement, a warmup, and the median of three runs.
It exercises all-hit, mixed, and all-miss inputs through `class(t_Boundary)`;
ray construction and input generation are outside the timed loop. Compare
revisions using the same benchmark source and its `[[example]]` entry in
`fpm.toml`. Checksums include positions, distances, and ray normals; hit counts
are also printed.

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

## Initial individual shape results

Measured on 2026-09-22 on `ng0002`, with the same compiler and flags described
above. The baseline is v0.7.0 (`946ac6d`); baseline and optimized code ran in the
same single-core allocation. All 30 shape/scene/query combinations produced
identical checksums and hit counts. These timings describe the first scalar
optimization pass; the subsequent improvements are measured below. These
timings are for the all-hit scene:

| Shape | Query (1,048,576 calls) | v0.7.0 (s) | Optimized (s) | Speedup |
| --- | --- | ---: | ---: | ---: |
| Triangle | Segment | 0.069300 | 0.024833 | 2.79× |
| Rectangle | Segment | 0.112274 | 0.047362 | 2.37× |
| Sphere | Ray | 0.076801 | 0.057085 | 1.35× |
| Cut sphere | Ray | 0.085563 | 0.062364 | 1.37× |
| Cylinder | Ray | 0.087194 | 0.055082 | 1.58× |
| Triangle | Ray | 0.103719 | 0.056707 | 1.83× |
| Rectangle | Ray | 0.143983 | 0.075980 | 1.90× |

Mixed-input segment queries improved by 2.96× for triangles and 2.64× for
rectangles; all-miss segment queries improved by 3.23× and 2.74× respectively.
Sphere, cut-sphere, and cylinder segment kernels were left unchanged. Normal
optimizations benefit ray hits: all-miss sphere rays measured 0.006572 s before
and 0.006971 s after (about 6% slower), while cut-sphere misses were unchanged.
Workload, compiler, and timing variation affect the net benefit.

The triangle change reuses a contiguous negative-direction vector, avoiding
repeated array packing in gfortran 8.5.0 without changing determinant arithmetic
or edge tolerances. Ray hits on exact built-in types avoid redundant normal
dispatch, and the default normal uses a fixed-size inner product. User-derived
types retain their `normal` and `pnormal` overrides. No public signatures,
derived-type definitions, cached geometry, or fast-math flags were introduced.

Debug and optimized suites both pass, including 40,768 new scalar compatibility
checks against the previous triangle formulas and dot-based normal orientation.
These cover custom normal overrides, mutable geometry and metadata, all three
axes, tangent/endpoint/parallel queries, and zero motion. All pre-existing
regression, BVH, and rotation checks also pass with runtime checks enabled.

## Further scalar optimization

The ray path now normalizes sphere normals with a fixed-size local `NORM2`
calculation, keeping the original scaling behavior. It also constructs complete
hit records in one assignment. For triangles, this avoids costly temporary
record copies generated by gfortran 8.5.0; rectangles benefit through their
triangle queries. Public APIs, geometric formulas, and subclass dispatch remain
unchanged.

Measured on 2026-09-22 on `ng0004`, gfortran 8.5.0, one allocated core, release
profile plus `-O3`. Both the first scalar pass and the current implementation
ran in the same allocation. All-hit ray timings for 1,048,576 calls:

| Shape | First scalar pass (s) | Current (s) | Additional speedup |
| --- | ---: | ---: | ---: |
| Sphere | 0.057700 | 0.039726 | 1.45× |
| Cut sphere | 0.062353 | 0.043693 | 1.43× |
| Triangle | 0.056444 | 0.028702 | 1.97× |
| Rectangle | 0.076754 | 0.049779 | 1.54× |

These additional changes leave the segment kernels and cylinder implementation
unchanged. Mixed-input ray speedups were 1.33×, 1.32×, 1.81×, and 1.38× for the
four shapes above. All-miss sphere rays measured 0.006980 s in the first pass and
0.006666 s currently; triangle and rectangle ray misses also improved.

All 30 benchmark checksums and hit counts match both the first pass and v0.7.0.
The scalar compatibility suite now passes 40,882 checks in debug and optimized
builds, including non-axis-aligned normals at very small and large scales,
zero-radius shapes, and user overrides. The full existing regression, BVH, and
rotation suites also pass. Results depend on the compiler and query distribution.

## Release validation (v0.7.1)

A clean release-candidate build with freshly fetched futils at
`c0198325afa389ec82bdbd2451e95dd1a0a788a8` passes the full gfortran 8.5.0 suite in
both debug and optimized configurations: 205,165 regression/compatibility checks
plus the pre-existing tests. The three benchmark examples also compile.

nvfortran 23.9 builds with default flags and with `-O3`, but the exact rotation
comparison fails at axis 2, case 1, check 2645. The same failure reproduces on
v0.7.0 with the same dependency. The scalar compatibility suite passes in both
builds. A build with `-O3 -Kieee -Mnofma -Mbounds` encounters the compiler's
internal `memsym_of_ast:unexp.ast` error in `ellipsoidXYZ_hit`. Full-suite runtime
validation for this release is therefore limited to gfortran.
