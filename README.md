# An Exploratory Assignment on Minimum Spanning Trees

## Introduction

Let the weight of a tree be the sum of the squares of its edges lengths. Given a set of points `P` in the unit square
let `W(P)` be the weight of the _minimum spanning tree (MST)_ of `P`, where an edge length is the Euclidean distance between its endpoints. If `P` consist of the four corners of the square, then `W(P)= 3`. _Gilbert_ and _Pollack_ proved that `W(P)` is `O(1)` and this was extended to an arbitrary number of dimensions by _Bern_ and _Eppstein_. While more recent divide-and-conquer approaches have show that `W(P) ≤ 4`, no point set is known with `W(P) > 3`, and hence it has been widely conjectured that `W(P) ≤ 3`. Later it was proved that `W(P) < 3.41` \[[1](./proof.pdf)\].

The objective of this empirical experiment is to check if `W(P) < 3.41` holds. In order to the previous theorem, we uniformaly at random generate points in the unite square `P` and compute the weight of the MST of these points. We do this with an increasing number of points in order to explore the solution space.

The results of this experiment can be found in the [report](./report/report.pdf).

## Installation

This project is build using [GHC](https://www.haskell.org/ghc/)(compiler) and [cabal](https://cabal.readthedocs.io/en/latest/index.html)(build tool).

The easiest way to install both is using [ghcup](https://gitlab.haskell.org/haskell/ghcup-hs)

``` sh
# Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC using ghcup
ghcup install ghc 8.8.4

# Install cabal using ghcup
ghcup install cabal
```

Finally, we need to compile the project. This may takes some minutes and requires internet connection. This project does not depend on any `.so` so it should be possible to compile it in any architecture that supports `ghc`.

```sh
# It may takes some minutes
$ cabal build
...
Preprocessing executable 'mst-experiment' for mst-experiment-0.1.0.0..
Building executable 'mst-experiment' for mst-experiment-0.1.0.0..
```

### Alternative Installation

This project is also prepared to be build with [Stack](https://docs.haskellstack.org/en/stable/README/).

Install Stack following the instructions from [here](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then run the following command:

```bash
$ stack build
...
Installing executable mst-experiment in /home/arnau/projects/haskell/mst-experiment/.stack-work/install/x86_64-linux-tinfo6/8e847b3b360c55e4f2b05724757e725ca7f55e7cb74ffe5cc2e613d4fe029b37/8.8.4/bin
Registering library for mst-experiment-0.1.0.0..
```

## Usage

Let's start by showing the available options:

```sh
$ cabal run mst-experiment -- --help
Usage: mst-experiment COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  run                      Run the experiment and plot the result.
  single                   Single run on the given graph size.
  estimate                 Estimate k(n) for the experiment.
```

The executable has three options

* run: runs the experiment and plots the result to the given file (this may takes several minutes).
* single: computes the weight of the MST on the given number of vertices
* estimate (ignore): internal option that is needed to optimize the runs.

For example, to run the experiment

```bash
cabal run mst-experiment -- run --file "output"
```

Another example, to run the experiment with a complete undirected graph of 2048 vertices

```bash
cabal run mst-experiment -- single --size 2048 --repetitions 5
```
