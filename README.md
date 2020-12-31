# An Exploratory Assignment on Minimum Spanning Trees

## Statement

Consider a complete, undirected graph with `C(n, 2) = n(n-1)/2` edges. Each edge has a weight, which is a real number chosen uniformly at random on `[0,1]`.

Your goal is to estimate how the expected weight of the minimum spanning tree grows as a function of `n` for such graphs. This will require implementing a minimum spanning tree algorithm as well as procedures that generate the appropiate random graphs.

Depending on the algorithm you use and your implementation, you may find that your program uses too much memory when `n` is large. To reduce memory when `n` is large, we suggest the following approach. In this setting, the minimum spanning tree is extremely unlikely to use any edge of weight greater than `k(n)` for some function `k(n)`. We can first estimate `k(n)` by using repeated runs for small values of `n` and then throw away edges of weight larger than `k(n)` when `n` is large. If you use this approach, be sure to explain why throwing away edges in this manner will not lead to a situation where the program finds a spanning tree that is not actually minimal.

Run your program for `n = 16,32,64,128,256,512,1024,2048,4096,8192`, and larger values if your program runs fast enough. Run your program at least five times for each value of `n` and take the average. You should present a table listing the average tree size for the values of `n` that your program runs successfully. What seems to be happenning to the average size of the minimum spanning tree as `n` grows ?

In addition, you should write one or two pages discussing your experiments in more depth. The discussion should reflect what you have learned from this assignment and might address the following topics.

* What minimmum spanning tree algorithm did you use, and why?
* What is the running time of your algorithm?
* If you chose to throw away edges, how did you determine `k(n)`, and how effective was this approach?
* Can you give a rought explanation for your results? (The limiting behaviour as `n` grows large can be proven rigorously, but it is very difficult; you need not attempt to prove any exact result.)
* Did you have any interesting experiences with the random number generator? Do you trust it?

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

Usage: mst-experiment [(-e|--estimate) | (-n|--size INT) [-r|--repetitions INT]]

Available options:
  -e,--estimate            Estimate k(n)
  -n,--size INT            Size of the complete undirected graph
  -r,--repetitions INT     Number of repetitions of the experiment (default: 5)
  -h,--help                Show this help text
```

For example, to run the experiment with a complete undirected graph of 4096 vertices:

```bash
cabal run mst-experiment --size 4096 --repetitions 1
```
