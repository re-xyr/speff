# Sp.Eff

Sp is the effects library that accompanies the [HOPE '22](https://icfp22.sigplan.org/home/hope-2022) talk *Effect Handlers in Scope, Evidently*.

Sp is implemented with the technique known in the Haskell community as the "[ReaderT pattern]", and in literature as "evidence passing" ([Xie et al 2020]). Basically, effect handlers are stored in a vector that is passed around in the program, so any call to effect operations is local. This makes the effect system more efficient than other traditionally used approaches, like `mtl`-style and free(r) monads.

Compared to [Xie et al 2020], Sp uses *arrays*, instead of lists, to implement the vector of handlers ("evidence vector"). In practice this is more efficient in most cases, because calling effect operations (reading from the vector), which is much more frequent, is O(1) rather than O(n).

The main feature of Sp is that it supports *scoped effects*, i.e. effect operations that can accept not only *values*, but also *computations*. The effect handler can call the computations multiple times (just like multi-shot resumptions), or change the behavior of the current effect inside the computation.

Sp uses a modified version of the multi-shot delimited control monad `Ctl` implemented in [Ev.Eff](https://hackage.haskell.org/package/eveff). The modified version supports embedding any first-order IO operations; the performance overhead is not detrimental. In our benchmarks, Sp performs better than many other popular effects libraries like `polysemy` and `fused-effects`, as well as Ev. Other parts of Sp is based on `cleff`, which uses a similar evidence-passing approach without delimited control, but with other useful features in production.

On the theory side, Sp demonstrates how to combine evidence-passing semantics with scoped effects. On the application side, Sp shows how to optimize evidence-passing in Haskell and how to integrate IO in an evidence-passing system. An implementation similar to Sp but instead based on [GHC native delcont] could be better suited for production in the future.

[ReaderT pattern]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
[Xie et al 2020]: https://dl.acm.org/doi/10.1145/3408981
[GHC native delcont]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7942