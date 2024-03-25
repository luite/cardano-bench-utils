# Cardano benchmark tools

## Introduction

This is a collection of small utilities that I used to analyze a regression
in the `cardano-node` software. The utilities are not yet ready for general
use, but we may eventually make full ..., or merge the functionality into
existing profiling tools.

## Preparation

### Non-Haskell libraries

For building `cardano-node`, you need the following
libraries visible through `pkg-config`:

    * `libsecp256k1`
    * `libsodium`
    * `libblst`
    * `OpenSSL`


### GHC

GHC 8.10.7 is required to run the `ghc8107` variants of the benchmarks.
GHC 9.6.4 for the `ghc964` variant. For some configurations a customized
variant of GHC 9.6.4 is needed. Get that from
(https://gitlab.haskell.org/luite/ghc) (XXX branch!)

```
$ ln -s /path/to/my/ghc-8.10.7 ghc/ghc-8.10.7
$ ln -s /path/to/my/ghc-pkg-8.10.7 ghc/ghc-pkg-8.10.7
$ ln -s /path/to/my/ghc-9.6.4 ghc/ghc-9.6.4
$ ln -s /path/to/my/ghc-pkg-9.6.4 ghc/ghc-pkg-9.6.4
```

### Eventlog2html

`eventlog2html` needs to be in the `PATH` for profiling configurations that
depend on the eventlog.

### Cardano-node

The `cardano-node` directory needs to contain a source repository of the
cardano node project. Be sure to use the correct branch. The
`luite/872-ghc96-profiles` contains the benchmark profile for `solo`
and `solo-xs`:

```
$ git clone --branch=luite/872-ghc96-profiles https://github.com/IntersectMBO/cardano-node.git
```

### Haskell dependencies

We use a large cabal project that contains most Haskell dependencies for
the purpose of benchmarking. This allows us to quickly make changes to
upstream packages and keep track of how the changes affect performance.

The dependencies are contained in `lib` subdirectories. The intended purpose of
the lib subdirectories is as follows:

  * `lib.orig`: untouched versions of the upstream libraries. Don't change these.
  * `lib.opt`: local modifications for benchmarking. A diff between `lib.opt`
     and `lib.orig` is generated for each benchmark run that uses `lib.opt`

To populate the `lib.orig` and `lib.opt` directores and generate the
corresponding `config/generated/libs-orig.project` and
`config/generated/libs-opt.project`, run the following command:

```
cabal run localize-dependencies
```

Note: Because of a limitation in the `localize-dependencies` program, updates
to `.cabal` metadata that may be present on the `hackage` and `CHaP`
repositories are not included in the `libs.orig` and `libs.old` directories.
If version constraints can't be solved because of this, add packages to the
`allow-newer` section in the `configs/include/default.project`.

# Usage


Use the `run-bench` program to run a benchmark profile:

```
cabal run run-bench solo-xs ghc964-orig-vanilla
```

Or you can run all the configurations for a single profile:

```
cabal run run-bench solo all
```

# Implementation Nodes/Remarks

This package is experimental and was put together quickly during an
investigation of `cardano-node` performance. There are some implementation
notes/remarks.

- I'm not sure if we always get a complete package list with
  `localize-dependencies`, perhaps we can force it to use an empty package db
  to start
- We need some fixes to `ghc-events` to correctly parse/emit GHC 9.6 events.
  Build `filterlog` and `eventlog2html` against this version!
- More postprocessing is needed, and cleaning of the results.
- we should add a lock file to prevent running multiple benchmarks at the same
  time (since we overwrite `cardano-node-service.nix` one run can influence
  results of another)
- can we find a way to benchmark without changing `cardano-node-service.nix`?

