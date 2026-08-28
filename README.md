AT
==

![build status](https://github.com/mvr/at/actions/workflows/haskell.yml/badge.svg)

A Haskell rewrite of
[Kenzo](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/), a
collection of algorithms for 'effective algebraic topology'. The
algorithms and implementations in Kenzo were created by Francis
Sergeraert, Julio Rubio Garcia, Xavier Dousson, Ana Romero and many
collaborators.

Writing it from scratch myself is the only chance I have of
understanding it!

Examples
--------
See the `examples/` folder.

```
> homotopyGroup 4 (Sphere 3)
Right ℤ/2

> homotopyGroup 6 (Sphere 3)
[ ... 30 seconds pass ... ]
Right ℤ/(2^2) ⊕ ℤ/3
```

```
> homology (Wbar (WbarDiscrete (Zmod 3)))
[ℤ,0,ℤ/3,0,ℤ/3,0,ℤ/(3^2),ℤ/3,ℤ/3,ℤ/3,ℤ/3 ⊕ ℤ/3,^C
```

Central Concepts of Kenzo
-------------------------

A *simplicial set* `X` is described by a type `a`, containing whatever
data is required to specify `X`, and a type `GeomSimplex a`, whose
elements correspond to non-degenerate simplices (in Kenzo called
'geometric simplices'). Like Kenzo we also allow a predicate on
`GeomSimplex a` specifying when an element actually describes a
geometric simplex and when it is 'spurious'.

An actual simplex of `X` is a geometric simplex together with a
'formal degeneracy operator', which is a list of degeneracy operators
in a normal form. Face maps are implemented as functions from
geometric simplices to (possibly degenerate) simplices, and the
extension of these face maps to all simplices is forced by the
simplicial identities.

A simplicial set is *of finite type* if there is a finite number of
geometric simplices for each dimension, and there is a function giving
a list of these simplices for any dimension `n`. It is not required
that there are finitely many geometric simplices overall.

The *normalised chain complex* `N(X)` of `X` has each `N(X)_n` given
by the free abelian group on the set of nondegenerate `n`-simplices of
`X`, with the boundary of a simplex calculated similar to usual (the
alternating sum of face maps), but ignoring any degenerate faces.

If `C(X)` is the ordinary chain complex of simplicial chains of `X`,
the quotient map `C(X) -> N(X)` is a quasi-isomorphism, and so if `X`
is of finite type, then the homology of `X` can be computed via
`N(X)`.

But many unavoidable simplicial sets (like `K(ℤ,n)` and loop spaces
`ΩX`) are not of finite type, and so we need some other way to
calculate their homology. This is where 'effective homology' comes in.

A *reduction* between chain complexes `C` and `D` is a strong
deformation retract of chain complexes. The data of a reduction
unwinds to a triple (`f : C -> D`, `g : D -> C`, `h : C -> C`) where
`f` and `g` are degree 0, the homotopy operator `h` is degree 1, and
certain equations involving these hold.  A *(strong chain)
equivalence* between two chain complexes `C` and `D` is a span of
reductions `l : E -> C` and `r : E -> D`.

An *effective homology structure* on `C` is an equivalence between `C`
and a chain complex `F` of finite type.

A *simplicial set with effective homology* is a simplicial set `X`
equipped with an effective homology structure on `N(X)`.

The point of Kenzo is that although constructions on simplicial sets
sometimes do not preserve levelwise finiteness, they *do* extend to
effective homology structures. And so if we begin with a finite
simplicial complex and perform some constructions using it, then we
can often compute the homology of the result even if the actual
simplicial sets are now far too complicated to get a handle on.

Plan
----

#### Homological Algebra
- Definitions
  - [x] Chain Complex
  - [x] Bicomplex
    - [x] Tot
  - [x] Reduction
    - [x] Perturbation
  - [x] Strong Equivalence
    - [x] Composition via span
- Constructions
  - [x] Tensor (of chain complex)
    - [x] Functoriality
  - [ ] Hom (of chain complex)
  - [x] 'Bicone' (specialised pushout for surjections)
  - [x] Bar
    - [x] Commutative algebra structure
    - [ ] Functoriality
  - [x] Cobar
    - [x] Of 1-reduced
    - [ ] Of 0-reduced
    - [ ] Functoriality

#### Simplicial Sets
- Definitions
  - [x] Simplicial Set
  - [x] Simplicial Morphism
  - [x] Simplicial Group
  - [ ] Kan Structure
  - [x] SSet With Effective Homology
  - [x] Principal Fibrations
  - [x] Discrete Vector Fields
  - [x] Coalgebra Structure on Chains
  - [x] Algebra Structure on Chains of Groups
  - [ ] Kan Structure on Chains of Groups
- Finite Examples
  - [x] Spheres
    - [x] Treat `S¹` separately (not 1-reduced)
  - [x] Moore Spaces
  - [x] Real Projective Spaces
  - [ ] Lens Spaces
- Eilenberg-MacLane Spaces
  - [x] [`K(ℤ,1)`](#ref-kendoc)
  - [x] `K(ℤ/2,1)` (Can be made particularly efficient)
  - [x] `K(ℤ/p,1)`
- Constructions
  - [x] Products
    - [x] Group structure
  - [x] Total Space of Principal Fibration
  - [ ] Loop Space
    - [ ] Of 1-reduced
    - [ ] Of 0-reduced
    - [ ] Group structure
    - [ ] Canonical twisting function `X -> GX`
  - [x] Classifying Space
    - [x] For 0-reduced group
    - [x] For non-reduced group
    - [x] Special case for discrete groups
    - [x] Group Structure
    - [x] Canonical twisting function `WG -> G`
  - [ ] Suspension
    - [ ] 0-reduced
    - [ ] General [Kan suspension](#ref-goerss-jardine)
  - [ ] Pushouts (of 1-reduced sSets)
  - [ ] Other Finite Homotopy Colimits
  - [ ] 'Nerve' taking a ChainComplex back to a sAb?

#### Effective Homology
- Classifying Spaces
  - [x] Direct Reduction of `K(ℤ,1)` to `S¹`
  - [x] [Of 0-reduced Abelian sGrps](#ref-sergeraert%3Advf-slides)
  - [ ] Of General sGrps
- Products
  - [x] Eilenberg-Zilber reduction
  - [ ] Use specialised contraction maps for efficiency
- Fibrations
  - Total Space from Base and Fibre ('Serre' problem)
    - [x] [1-reduced Fibre](#ref-as%3Advf)
    - [ ] [0-reduced Fibre](#ref-filakovsky%3Atwisted-products)
  - [ ] Fibre from Base and Total Space ('Eilenberg-Moore' problem)
- Loop Space
  - [ ] [1-reduced](#ref-kendoc)
  - [ ] [0-reduced](#ref-hess-tonks%3Aloop-group)
- Colimits
  - [ ] Suspension
  - [ ] [Pushouts (of 1-reduced sSets)](#ref-heras%3Apushout)
  - [ ] [Finite Homotopy Colimits](#ref-filakovsky%3Ahocolim)
- Discrete Vector Fields
  - [x] Induced Reduction
  - [x] Products
  - [x] Fibrations
  - [x] `K(ℤ,1)`
    - [x] [Naive but easy](#ref-kms%3Apoly-em-spaces)
    - [ ] [Polynomial time but complicated](#ref-kms%3Apoly-em-spaces)
  - [x] `K(ℤ/n,1)`
  - [x] Classifying Spaces for 0-reduced sAb
- Homotopy Groups
  - [x] [Whitehead Tower (for 1-reduced sSet)](#ref-real%3Ahomotopy-groups)
  - [ ] [Postnikov Tower?](#ref-ckmvw%3Apoly-homotopy-groups)
- Cohomology Operations
  - [ ] [Over fields using  "minimal models"](#ref-gr%3Acohomology-ops)

#### Misc TODOs
- [ ] Fix space leaks, jeez
- [ ] Pretty-printing for everything (unicode sub/superscripts in output?)
- [ ] Docs for everything
- [ ] Move this list to Github issues
- [ ] Consolidate some files? Eg. Sum, Shift into ChainComplex
- [x] Use bit operations eg from
      [bits-extra](https://github.com/haskell-works/bits-extra) for
      degeneracy operators.
- [ ] Short-circuits: e.g. composing with zero/id for
      morphism/reduction/equivalence
- [ ] Make sure things are being aggressively inlined
- [ ] Make homology calculation do less work: should just need SNF
      of one matrix and the rank of another.
- [ ] Improve Smith normal form code, would be better to call out to
      some existing library instead of rolling our own. The options
      appear to be [LinBox](https://linalg.org/) or
      [FLINT](http://flintlib.org/). The former appears to support
      sparse matrices better
- [ ] Check homology of `K(G,n)` calculations against known results
      <!-- eg [Clement's thesis](#ref-clement%3Athesis) -->
- [x] Add methods to produce representatives of homology classes
- [ ] Rewrite `Bar` to be a perturbed `TensorCoalgebra`?
- [ ] Rename `basepoint` to `geomBasepoint` say

Notes
-----

- I have switched a little terminology: I believe Kenzo uses
  'effective' for finite-type things and 'locally effective' for
  what I am calling effective things, but I find this a bit confusing.
- In Kenzo, every sSet is conflated with its chain complex of
  normalised chains, here I have kept the two separate.
- Avoid over-engineering the Haskell as much as possible.
- That being said, the use of `Constrained.Category` is a bit of a
  mess.
- There may be a way to unify some of the algorithms via bicomplexes
  and the ['generalised Eilenberg-Zilber
  theorem'](https://ncatlab.org/nlab/show/Eilenberg-Zilber+theorem)
  relating the diagonal and total complexes. But the EZ-theorem only
  gives a strong deformation retract in special cases, in general it
  is just a chain homotopy equivalence.
- The classifying space functor `Wbar` factors through the 'total
  bisimplicial set' functor. But it would be difficult to describe the
  total functor on bisimplicial spaces algorithmically, because its
  definition involves the equaliser of certain face maps. So it only
  makes sense to implement bicomplexes and not bisimplicial sets.
- Auto-formatting the code:
  `fourmolu -o -XTypeApplications -i $(find . -name '*.hs')`
- Running Kenzo with SBCL:
  ```
  > rlwrap sbcl
  (require :asdf)
  (load "kenzo.asd")
  (asdf:load-system "kenzo")
  (in-package :kenzo)

  (finite-ss-table '(a b 1 c (b a)))
  ```
  etc.
- classes.lisp in Kenzo contains the meaning of some of the 4 letter
  abbreviations
  * ABSM = ABstract SiMplex
  * GMSM = GeoMetric SiMplex
  * CMBN = CoMBinatioN
  * CFFC = CoeFFiCient
  * GNRT = GeNeRaTor
  * CMPR = CoMPaRison
  * CMPRF = CoMPaRison Function
  * ICMBN = Internal-CoMBiNation
  * STRT = STRaTegy
  * bsgn = BaSe GeNerator
  * dffr = DiFFeRential
  * grmd = GRound MoDule
  * efhm = EFfective HoMology
  * idnm = IDentification NuMber
  * orgn = ORiGiN
  * vctr = VeCToR
  * intr-mrph = INTeRnal-MoRPHism (the class of actual functions
    implementing a morphism of simplicial sets or chain complexes)
  * sbtr = SuBTRact
  * crpr = CarRtesian PRoduct
  * BRGN = BaR GeNerator
  * TNPR = TeNsor PRoduct
- Unguessable CL functions
  * (ash x n) = bit shift x left by n
  * (add x y) and (sbtr x y) can sometimes actually be a use of the
    perturbation lemma(!!), depending on the types of the arguments.

References
----------

### Code:
* [Kenzo homepage](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/)
* [Kenzo documentation](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/Kenzo-doc.pdf), likely out of date with the code in places
* ['Official' Kenzo mirror on GitHub](https://github.com/gheber/kenzo), there are three different
  versions of the code [here](https://github.com/gheber/kenzo/tree/master/src), I am not clear on what is
  gained/lost between them. There are online [Jypter
  notebooks](https://sur-l-analysis-sit.us/) that let you play with Kenzo (I believe Kenzo-9) without
  having to figure out how to install and operate a Common Lisp
  environment
* [Fork by Ana Romero + collaborators](https://github.com/miguelmarco/kenzo), has some added features
  over the mirror above, worth looking at [resolutions.lisp](https://github.com/miguelmarco/kenzo/blob/master/src/anromero/resolutions.lisp) and
  [homotopy.lisp](https://github.com/miguelmarco/kenzo/blob/master/src/anromero/homotopy.lisp), but is missing all the discrete vector field code
* [Modules written by Ana Romero](https://github.com/ana-romero/Kenzo-external-modules), mostly to do with spectral
  sequences

### Papers:
Everything even remotely relevant to effective algebraic topology that
I can find (not all of which is relevant for implementation). Some of
the documents have multiple versions; I have tried to link to the most
recent in each case. Some material is repeated in different
references.

<!-- To generate: pandoc kenzo.bib -C --csl=association-for-computing-machinery.csl -t gfm -o out.md -->
<div id="refs" class="references csl-bib-body" data-entry-spacing="0">

<div id="ref-henry:quasicategory-rewriting" class="csl-entry">

<span class="csl-left-margin">\[1\]
</span><span class="csl-right-inline">Simon Henry. 2026. Rewriting and
presentations of quasicategories.
<https://doi.org/10.48550/arXiv.2608.02529></span>

</div>

<div id="ref-miguel:thesis" class="csl-entry">

<span class="csl-left-margin">\[2\]
</span><span class="csl-right-inline">Daniel Miguel Treviño. 2026.
Spectral systems: New instances and algorithms. PhD thesis. Universidad
de La Rioja. Retrieved from
<https://dialnet.unirioja.es/descarga/tesis/402493.pdf></span>

</div>

<div id="ref-gbg:constructive-alexander-duality" class="csl-entry">

<span class="csl-left-margin">\[3\]
</span><span class="csl-right-inline">Aldo Gonzalez-Lorenzo, Alexandra
Bac, and Yann-Situ Gazull. 2025. A constructive approach of Alexander
duality. *Journal of Applied and Computational Topology* 9, 1 (2025), 2.
<https://doi.org/10.1007/s41468-024-00198-1></span>

</div>

<div id="ref-bg:effective-kan-fibrations" class="csl-entry">

<span class="csl-left-margin">\[4\]
</span><span class="csl-right-inline">Benno van den Berg and Freek
Geerligs. 2025. Examples and cofibrant generation of effective Kan
fibrations. *Journal of Pure and Applied Algebra* 229, 1 (2025), 107812.
<https://doi.org/10.1016/j.jpaa.2024.107812></span>

</div>

<div id="ref-simkova:rational-homotopy" class="csl-entry">

<span class="csl-left-margin">\[5\]
</span><span class="csl-right-inline">Mária Šimková. 2025. Rational
homotopy equivalence. <https://doi.org/10.48550/arXiv.2512.21182></span>

</div>

<div id="ref-sergeraert:kannan-bachem" class="csl-entry">

<span class="csl-left-margin">\[6\]
</span><span class="csl-right-inline">Francis Sergeraert. 2024. About
the Kannan-Bachem algorithm.
<https://doi.org/10.48550/arXiv.2411.02422></span>

</div>

<div id="ref-mbr:universal-covers" class="csl-entry">

<span class="csl-left-margin">\[7\]
</span><span class="csl-right-inline">Miguel Angel Marco-Buzunariz and
Ana Romero. 2024. Computing the homology of universal covers via
effective homology and discrete vector fields. (2024). Retrieved from
<https://arxiv.org/abs/2409.06357></span>

</div>

<div id="ref-vokrinek:enriched-hpt" class="csl-entry">

<span class="csl-left-margin">\[8\]
</span><span class="csl-right-inline">Lukáš Vokřínek. 2024. Enriched
categorical aspects of homological perturbation theory.
<https://doi.org/10.48550/arXiv.2412.21182></span>

</div>

<div id="ref-mnev:kz2" class="csl-entry">

<span class="csl-left-margin">\[9\]
</span><span class="csl-right-inline">Nikolai Mnëv. 2024. $`K(Z,2)`$ out
of circular permutations. Retrieved from
<https://arxiv.org/abs/2406.01625></span>

</div>

<div id="ref-clrs:h-regularize" class="csl-entry">

<span class="csl-left-margin">\[10\]
</span><span class="csl-right-inline">Julián Cuevas-Rozo, Laureano
Lambán, Ana Romero, and Humberto Sarria. 2023. A new method to
$`h`$-regularize finite topological spaces. *Discrete Mathematics* 346,
12 (2023), 113636. <https://doi.org/10.1016/j.disc.2023.113636></span>

</div>

<div id="ref-cmmm:cartan-odd" class="csl-entry">

<span class="csl-left-margin">\[11\]
</span><span class="csl-right-inline">Federico Cantero-Morán and Anibal
M. Medina-Mardones. 2023. An effective proof of the cartan formula: Odd
primes. Retrieved from <https://arxiv.org/abs/2305.08973></span>

</div>

<div id="ref-fv:homotopy-classes-diagrams" class="csl-entry">

<span class="csl-left-margin">\[12\]
</span><span class="csl-right-inline">Marek Filakovský and Lukáš
Vokřínek. 2023. Computing homotopy classes for diagrams. *Discrete &
Computational Geometry* 70, 3 (2023), 866–920.
<https://doi.org/10.1007/s00454-023-00513-0></span>

</div>

<div id="ref-clrs:finite-topological-spaces" class="csl-entry">

<span class="csl-left-margin">\[13\]
</span><span class="csl-right-inline">Julián Cuevas-Rozo, Laureano
Lambán, Ana Romero, and Humberto Sarria. 2023. Effective homological
computations on finite topological spaces. *Applicable Algebra in
Engineering, Communication and Computing* 34, 1 (2023), 33–56.
<https://doi.org/10.1007/s00200-020-00462-8></span>

</div>

<div id="ref-mgrr:spectral-systems" class="csl-entry">

<span class="csl-left-margin">\[14\]
</span><span class="csl-right-inline">Daniel Miguel, Andrea Guidolin,
Ana Romero, and Julio Rubio. 2023. Effective spectral systems relating
serre and eilenberg–moore spectral sequences. *Journal of Symbolic
Computation* 114, (2023), 122–148.
<https://doi.org/10.1016/j.jsc.2022.04.014></span>

</div>

<div id="ref-mgrr:generalized-serre-systems" class="csl-entry">

<span class="csl-left-margin">\[15\]
</span><span class="csl-right-inline">Daniel Miguel, Andrea Guidolin,
Ana Romero, and Julio Rubio. 2022. A generalization of effective Serre
spectral systems for $`m`$-multicomplexes. In *Proceedings of the XVII
EACA: Encuentros de álgebra computacional y aplicaciones*, 2022.
Castelló de la Plana, Spain, 125–128. Retrieved from
<https://drive.google.com/file/d/1lTgpyfDNWuIBY60lMz49cy3CIcTYkjWu/view></span>

</div>

<div id="ref-medina-mardones:axiomatic-steenrod" class="csl-entry">

<span class="csl-left-margin">\[16\]
</span><span class="csl-right-inline">Anibal M. Medina-Mardones. 2022.
An axiomatic characterization of Steenrod’s cup-$`i`$ products.
Retrieved from <https://arxiv.org/abs/1810.06505></span>

</div>

<div id="ref-ss:dvf-monomial-resolutions" class="csl-entry">

<span class="csl-left-margin">\[17\]
</span><span class="csl-right-inline">Eduardo Sáenz-de-Cabezón and
Francis Sergeraert. 2022. Discrete vector fields for monomial
resolutions. In *Applications of computer algebra – ACA 2022*, 2022.
Gebze-Istanbul, Turkey, 75–77. Retrieved from
<https://www.math.unm.edu/~aca/ACA/2022/scale.gtu.edu.tr/files/aca_book.pdf></span>

</div>

<div id="ref-medina-mardones:steenrod-formulas" class="csl-entry">

<span class="csl-left-margin">\[18\]
</span><span class="csl-right-inline">Anibal M. Medina-Mardones. 2022.
New formulas for cup-$`i`$ products and fast computation of Steenrod
squares. Retrieved from <https://arxiv.org/abs/2105.08025></span>

</div>

<div id="ref-mgrr:new-spectral-systems" class="csl-entry">

<span class="csl-left-margin">\[19\]
</span><span class="csl-right-inline">Daniel Miguel, Andrea Guidolin,
Ana Romero, and Julio Rubio. 2021. Constructing new spectral systems
from simplicial fibrations. *ACM Communications in Computer Algebra* 55,
3 (September 2021), 87–91.
<https://doi.org/10.1145/3511528.3511534></span>

</div>

<div id="ref-km:cochain-may-steenrod" class="csl-entry">

<span class="csl-left-margin">\[20\]
</span><span class="csl-right-inline">Ralph M. Kaufmann and Anibal M.
Medina-Mardones. 2021. Cochain level May-Steenrod operations. *Forum
Math.* 33, 6 (2021), 1507–1526.
<https://doi.org/10.1515/forum-2020-0296></span>

</div>

<div id="ref-gr:leray-serre" class="csl-entry">

<span class="csl-left-margin">\[21\]
</span><span class="csl-right-inline">Andrea Guidolin and Ana Romero.
2021. Computing higher Leray-Serre spectral sequences of towers of
fibrations. *Foundations of Computational Mathematics* 21, 4 (2021),
1023–1074. <https://doi.org/10.1007/s10208-020-09475-8></span>

</div>

<div id="ref-gdrv:multipersistence" class="csl-entry">

<span class="csl-left-margin">\[22\]
</span><span class="csl-right-inline">Andrea Guidolin, Jose Divasón, Ana
Romero, and Francesco Vaccarino. 2021. Computing invariants for
multipersistence via spectral systems and effective homology. *Journal
of Symbolic Computation* 104, (2021), 724–753.
<https://doi.org/10.1016/j.jsc.2020.09.007></span>

</div>

<div id="ref-cdmr:kenzo-sagemath" class="csl-entry">

<span class="csl-left-margin">\[23\]
</span><span class="csl-right-inline">Julián Cuevas-Rozo, Jose Divasón,
Miguel Marco-Buzunáriz, and Ana Romero. 2021. Integration of the Kenzo
system within SageMath for new algebraic topology computations.
*Mathematics* 9, 7 (2021), 722.
<https://doi.org/10.3390/math9070722></span>

</div>

<div id="ref-franz:twisting" class="csl-entry">

<span class="csl-left-margin">\[24\]
</span><span class="csl-right-inline">Matthias Franz. 2021. Szczarba’s
twisting cochain and the Eilenberg-Zilber maps. *Collectanea
Mathematica* 72, 3 (2021), 569–586.
<https://doi.org/10.1007/s13348-020-00299-x></span>

</div>

<div id="ref-rrss:em-spectral-sequence" class="csl-entry">

<span class="csl-left-margin">\[25\]
</span><span class="csl-right-inline">Ana Romero, Julio Rubio, Francis
Sergeraert, and Markus Szymik. 2020. A new Kenzo module for computing
the Eilenberg-Moore spectral sequence. *ACM Communications in Computer
Algebra* 54, 2 (2020), 57–60.
<https://doi.org/10.1145/3427218.3427225></span>

</div>

<div id="ref-chen:thesis" class="csl-entry">

<span class="csl-left-margin">\[26\]
</span><span class="csl-right-inline">Ruian Chen. 2020.
$`E_\infty`$-Rings and Modules in Kan Spectral Sheaves. PhD thesis.
University of Michigan. <https://doi.org/2027.42/155195></span>

</div>

<div id="ref-zhechev:thesis" class="csl-entry">

<span class="csl-left-margin">\[27\]
</span><span class="csl-right-inline">Stephan Zhechev. 2019. Algorithmic
aspects of homotopy theory and embeddability. PhD thesis. Institute of
Science; Technology Austria.
<https://doi.org/10.15479/AT:ISTA:6681></span>

</div>

<div id="ref-medina-mardones:cartan-even" class="csl-entry">

<span class="csl-left-margin">\[28\]
</span><span class="csl-right-inline">Anibal M. Medina-Mardones. 2019.
An effective proof of the cartan formula: The even prime. Retrieved from
<https://arxiv.org/abs/1907.12113></span>

</div>

<div id="ref-rrs:fibrations-implementation" class="csl-entry">

<span class="csl-left-margin">\[29\]
</span><span class="csl-right-inline">Ana Romero, Julio Rubio, and
Francis Sergeraert. 2019. An implementation of effective homotopy of
fibrations. *Journal of Symbolic Computation* 94, (2019), 149–172.
<https://doi.org/10.1016/j.jsc.2018.08.001></span>

</div>

<div id="ref-as:ez-dvf" class="csl-entry">

<span class="csl-left-margin">\[30\]
</span><span class="csl-right-inline">Ana Romero and Francis Sergeraert.
2019. The Eilenberg-Zilber theorem via discrete vector fields. Retrieved
from
<https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/EZ-submitted.pdf></span>

</div>

<div id="ref-vj:ainfty-algorithms" class="csl-entry">

<span class="csl-left-margin">\[31\]
</span><span class="csl-right-inline">Mikael Vejdemo-Johansson. 2018.
Algorithms in $`A^\infty`$-algebras. *Georgian Mathematical Journal* 25,
4 (2018), 629–635. <https://doi.org/10.1515/gmj-2018-0057></span>

</div>

<div id="ref-sergeraert:hexagonal-lemma" class="csl-entry">

<span class="csl-left-margin">\[32\]
</span><span class="csl-right-inline">Francis Sergeraert. 2018. The
homological hexagonal lemma. *Georgian Mathematical Journal* 25, 4
(2018), 603–622. <https://doi.org/10.1515/gmj-2018-0055></span>

</div>

<div id="ref-rs:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[33\]
</span><span class="csl-right-inline">Ana Romero and Francis Sergeraert.
2017. A Bousfield-Kan algorithm for computing the *effective* homotopy
of a space. *Foundations of Computational Mathematics* 17, 5 (2017),
1335–1366. <https://doi.org/10.1007/s10208-016-9322-z></span>

</div>

<div id="ref-ckp:kan-spectra" class="csl-entry">

<span class="csl-left-margin">\[34\]
</span><span class="csl-right-inline">Ruian Chen, Igor Kriz, and Ales
Pultr. 2017. Kan’s combinatorial spectra and their sheaves revisited.
*Theory and Applications of Categories* 32, (2017), No. 39, 1363–1396.
Retrieved from
<http://www.tac.mta.ca/tac/volumes/32/39/32-39abs.html></span>

</div>

<div id="ref-hess:twisting-cochain" class="csl-entry">

<span class="csl-left-margin">\[35\]
</span><span class="csl-right-inline">Kathryn Hess. 2016. The Hochschild
complex of a twisting cochain. *Journal of Algebra* 451, (2016),
302–356. <https://doi.org/10.1016/j.jalgebra.2015.11.040></span>

</div>

<div id="ref-rs:iterated-loop" class="csl-entry">

<span class="csl-left-margin">\[36\]
</span><span class="csl-right-inline">Ana Romero and Francis Sergeraert.
2015. A combinatorial tool for computing the effective homotopy of
iterated loop spaces. *Discrete & Computational Geometry* 53, 1 (2015),
1–15. <https://doi.org/10.1007/s00454-014-9650-1></span>

</div>

<div id="ref-filakovsky:thesis" class="csl-entry">

<span class="csl-left-margin">\[37\]
</span><span class="csl-right-inline">Marek Filakovský. 2015.
Algorithmic construction of the postnikov tower for diagrams of
simplicial sets. PhD thesis. Masaryk University. Retrieved from
<http://www.math.muni.cz/~filakovsky/THESIS2.pdf></span>

</div>

<div id="ref-coq-fpm" class="csl-entry">

<span class="csl-left-margin">\[38\]
</span><span class="csl-right-inline">Cyril Cohen and Anders Mörtberg.
2014. A coq formalization of finitely presented modules. In *Interactive
theorem proving*, 2014. Springer International Publishing, 193–208.
<https://doi.org/10.1007/978-3-319-08970-6_13></span>

</div>

<div id="ref-ckmsvw:maps-into-sphere" class="csl-entry">

<span class="csl-left-margin">\[39\]
</span><span class="csl-right-inline">Martin Čadek, Marek Krčál, Jiří
Matoušek, Francis Sergeraert, Lukáš Vokřínek, and Uli Wagner. 2014.
Computing all maps into a sphere. *Journal of the ACM* 61, 3 (2014),
Art. 17, 44. <https://doi.org/10.1145/2597629></span>

</div>

<div id="ref-filakovsky:hocolim" class="csl-entry">

<span class="csl-left-margin">\[40\]
</span><span class="csl-right-inline">Marek Filakovský. 2014. Effective
homology for homotopy colimit and cofibrant replacement. *Universitatis
Masarykianae Brunensis. Facultas Scientiarum Naturalium. Archivum
Mathematicum* 50, 5 (2014), 273–286.
<https://doi.org/10.5817/AM2014-5-273></span>

</div>

<div id="ref-ckmvw:poly-homotopy-groups" class="csl-entry">

<span class="csl-left-margin">\[41\]
</span><span class="csl-right-inline">Martin Čadek, Marek Krčál, Jiří
Matoušek, Lukáš Vokřínek, and Uli Wagner. 2014. Polynomial-time
computation of homotopy groups and Postnikov systems in fixed dimension.
*SIAM Journal on Computing* 43, 5 (2014), 1728–1780.
<https://doi.org/10.1137/120899029></span>

</div>

<div id="ref-sergeraert:dvf-slides" class="csl-entry">

<span class="csl-left-margin">\[42\]
</span><span class="csl-right-inline">Francis Sergeraert. 2013. Discrete
vector fields and fundamental algebraic topology. Retrieved from
<https://www-fourier.ujf-grenoble.fr/~sergerar/Talks/13-04-Tokyo.pdf></span>

</div>

<div id="ref-kms:poly-em-spaces" class="csl-entry">

<span class="csl-left-margin">\[43\]
</span><span class="csl-right-inline">Marek Krčál, Jiří Matoušek, and
Francis Sergeraert. 2013. Polynomial-time homology for simplicial
Eilenberg-MacLane spaces. *Foundations of Computational Mathematics. The
Journal of the Society for the Foundations of Computational Mathematics*
13, 6 (2013), 935–963.
<https://doi.org/10.1007/s10208-013-9159-7></span>

</div>

<div id="ref-rr:homology-of-groups" class="csl-entry">

<span class="csl-left-margin">\[44\]
</span><span class="csl-right-inline">Ana Romero and Julio Rubio. 2012.
Computing the homology of groups: The geometric way. *Journal of
Symbolic Computation* 47, 7 (2012), 752–770.
<https://doi.org/10.1016/j.jsc.2011.12.007></span>

</div>

<div id="ref-rs:constructive-homology" class="csl-entry">

<span class="csl-left-margin">\[45\]
</span><span class="csl-right-inline">Julio Rubio and Francis
Sergeraert. 2012. Constructive homological algebra and applications.
Retrieved from <https://arxiv.org/abs/1208.3816></span>

</div>

<div id="ref-stevenson:decalage" class="csl-entry">

<span class="csl-left-margin">\[46\]
</span><span class="csl-right-inline">Danny Stevenson. 2012. Décalage
and Kan’s simplicial loop group functor. *Theory and Applications of
Categories* 26, (2012), No. 28, 768–787. Retrieved from
<http://www.tac.mta.ca/tac/volumes/26/28/26-28abs.html></span>

</div>

<div id="ref-as:dvf" class="csl-entry">

<span class="csl-left-margin">\[47\]
</span><span class="csl-right-inline">Ana Romero and Francis Sergeraert.
2012. Discrete vector fields and fundamental algebraic topology.
Retrieved from
<https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Vector-Fields.pdf></span>

</div>

<div id="ref-filakovsky:twisted-products" class="csl-entry">

<span class="csl-left-margin">\[48\]
</span><span class="csl-right-inline">Marek Filakovský. 2012. Effective
chain complexes for twisted products. *Universitatis Masarykianae
Brunensis. Facultas Scientiarum Naturalium. Archivum Mathematicum* 48, 5
(2012), 313–322. <https://doi.org/10.5817/AM2012-5-313></span>

</div>

<div id="ref-rs:homotopy-fibrations" class="csl-entry">

<span class="csl-left-margin">\[49\]
</span><span class="csl-right-inline">Ana Romero and Francis Sergeraert.
2012. Effective homotopy of fibrations. *Applicable Algebra in
Engineering, Communication and Computing* 23, 1-2 (2012), 85–100.
<https://doi.org/10.1007/s00200-012-0168-6></span>

</div>

<div id="ref-spiwack:thesis" class="csl-entry">

<span class="csl-left-margin">\[50\]
</span><span class="csl-right-inline">Arnaud Spiwack. 2011.
<span class="nocase">Verified Computing in Homological Algebra</span>.
PhD thesis. Ecole Polytechnique X. Retrieved from
<https://pastel.archives-ouvertes.fr/pastel-00605836></span>

</div>

<div id="ref-heras:pushout" class="csl-entry">

<span class="csl-left-margin">\[51\]
</span><span class="csl-right-inline">Jónathan Heras. 2010. Pushout
construction for the Kenzo systems. Retrieved from
<https://www.unirioja.es/cu/joheras/pushout/Doc/pushout.pdf></span>

</div>

<div id="ref-brs:a-infty" class="csl-entry">

<span class="csl-left-margin">\[52\]
</span><span class="csl-right-inline">Ainhoa Berciano Alcaraz, Julio
Rubio, and Francis Sergeraert. 2010. A case study of
$`A_\infty`$-structure. *Georgian Mathematical Journal* 17, 1 (2010),
57–77. <https://doi.org/10.1515/gmj.2010.003></span>

</div>

<div id="ref-real:twisted-ez" class="csl-entry">

<span class="csl-left-margin">\[53\]
</span><span class="csl-right-inline">Víctor Álvarez, José Andrés
Armario, María Dolores Frau, and Pedro Real. 2010. Cartan’s
constructions and the twisted Eilenberg-Zilber theorem. *Georgian
Mathematical Journal* 17, 1 (2010), 13–23.
<https://doi.org/10.1515/gmj.2010.006></span>

</div>

<div id="ref-romero:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[54\]
</span><span class="csl-right-inline">Ana Romero. 2010. Computing the
first stages of the Bousfield-Kan spectral sequence. *Applicable Algebra
in Engineering, Communication and Computing* 21, 3 (2010), 227–248.
<https://doi.org/10.1007/s00200-010-0123-3></span>

</div>

<div id="ref-heras:pushout-conf" class="csl-entry">

<span class="csl-left-margin">\[55\]
</span><span class="csl-right-inline">Jónathan Heras. 2010. Effective
homology of the pushout of simplicial sets. In *Proceedings of the XII
encuentros de álgebra computacional y aplicaciones*, 2010. 152–156.
Retrieved from <https://arxiv.org/abs/1410.3651></span>

</div>

<div id="ref-hprr:integrating-sources" class="csl-entry">

<span class="csl-left-margin">\[56\]
</span><span class="csl-right-inline">Jónathan Heras, Vico Pascual, Ana
Romero, and Julio Rubio. 2010. Integrating multiple sources to answer
questions in algebraic topology. In *Proceedings of the 10th ASIC and
9th MKM international conference, and 17th calculemus conference on
intelligent computer mathematics* (*AISC’10/MKM’10/calculemus’10*),
2010. Springer-Verlag, Paris, France, 331–335. Retrieved from
<https://arxiv.org/abs/1005.0749></span>

</div>

<div id="ref-hess-tonks:loop-group" class="csl-entry">

<span class="csl-left-margin">\[57\]
</span><span class="csl-right-inline">Kathryn Hess and Andrew Tonks.
2010. The loop group and the cobar construction. *Proceedings of the
American Mathematical Society* 138, 5 (2010), 1861–1876.
<https://doi.org/10.1090/S0002-9939-09-10238-1></span>

</div>

<div id="ref-real:algebra-structures" class="csl-entry">

<span class="csl-left-margin">\[58\]
</span><span class="csl-right-inline">V. Álvarez, J. A. Armario, M. D.
Frau, and P. Real. 2009. Algebra structures on the comparison of the
reduced bar construction and the reduced $`W`$-construction.
*Communications in Algebra* 37, 10 (2009), 3643–3665.
<https://doi.org/10.1080/00927870902747662></span>

</div>

<div id="ref-rer:classifying-space" class="csl-entry">

<span class="csl-left-margin">\[59\]
</span><span class="csl-right-inline">Ana Romero, Graham Ellis, and
Julio Rubio. 2009. Interoperating between computer algebra systems:
Computing homology of groups with Kenzo and GAP. In *ISSAC
2009—Proceedings of the 2009 International Symposium on Symbolic and
Algebraic Computation*, 2009. ACM, New York, 303–310.
<https://doi.org/10.1145/1576702.1576744></span>

</div>

<div id="ref-sergeraert:cp-spaces" class="csl-entry">

<span class="csl-left-margin">\[60\]
</span><span class="csl-right-inline">Francis Sergeraert. 2009.
Triangulations of complex projective spaces. Retrieved from
<https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Mirian.pdf></span>

</div>

<div id="ref-br:homalg" class="csl-entry">

<span class="csl-left-margin">\[61\]
</span><span class="csl-right-inline">Mohamed Barakat and Daniel
Robertz. 2008. `homalg`: A meta-package for homological algebra. *J.
Algebra Appl.* 7, 3 (2008), 299–317.
<https://doi.org/10.1142/S0219498808002813></span>

</div>

<div id="ref-thomas:wbar" class="csl-entry">

<span class="csl-left-margin">\[62\]
</span><span class="csl-right-inline">Sebastian Thomas. 2008. The
functors $`\bar{W}`$ and $`\text{Diag} \circ \text{Nerve}`$ are
simplicially homotopy equivalent. *Journal of Homotopy and Related
Structures* 3, 1 (2008), 359–378. Retrieved from
<https://arxiv.org/abs/0804.1082></span>

</div>

<div id="ref-rrs:computing-spectral-sequences" class="csl-entry">

<span class="csl-left-margin">\[63\]
</span><span class="csl-right-inline">A. Romero, J. Rubio, and F.
Sergeraert. 2006. Computing spectral sequences. *Journal of Symbolic
Computation* 41, 10 (2006), 1059–1079.
<https://doi.org/10.1016/j.jsc.2006.06.002></span>

</div>

<div id="ref-drs:modeling-inheritance" class="csl-entry">

<span class="csl-left-margin">\[64\]
</span><span class="csl-right-inline">César Domínguez, Julio Rubio, and
Francis Sergeraert. 2006. Modeling inheritance as coercion in the Kenzo
system. *Journal of Universal Computer Science* 12, 12 (2006),
1701–1730. <https://doi.org/10.3217/jucs-012-12-1701></span>

</div>

<div id="ref-real:reducing-costs" class="csl-entry">

<span class="csl-left-margin">\[65\]
</span><span class="csl-right-inline">Ainhoa Berciano, María José
Jiménez, and Pedro Real. 2006. Reducing computational costs in the basic
perturbation lemma. In *Computer algebra in scientific computing*,
Victor G. Ganzha, Ernst W. Mayr and Evgenii V. Vorozhtsov (eds.).
Springer, Berlin, 33–48. <https://doi.org/10.1007/11870814_3></span>

</div>

<div id="ref-gr:cohomology-ops" class="csl-entry">

<span class="csl-left-margin">\[66\]
</span><span class="csl-right-inline">R. González-Díaz and P. Real.
2003. Computation of cohomology operations of finite simplicial
complexes. In *Homology Homotopy Appl.* 83–93.
<https://doi.org/10.4310/HHA.2003.v5.n2.a4></span>

</div>

<div id="ref-clement:thesis" class="csl-entry">

<span class="csl-left-margin">\[67\]
</span><span class="csl-right-inline">Alain Clément. 2002. Integral
cohomology of finite Postnikov towers. PhD thesis. Université de
Lausanne. Retrieved from <https://doc.rero.ch/record/482></span>

</div>

<div id="ref-jimenez-real:coalgebra" class="csl-entry">

<span class="csl-left-margin">\[68\]
</span><span class="csl-right-inline">M. J. Jiménez and P. Real. 2001.
“Coalgebra” structures on 1-homological models for commutative
differential graded algebras. In *Computer algebra in scientific
computing (Konstanz, 2001)*. Springer, Berlin, 347–361.
<https://doi.org/10.1007/978-3-642-56666-0_26></span>

</div>

<div id="ref-gonzalez-diaz:thesis" class="csl-entry">

<span class="csl-left-margin">\[69\]
</span><span class="csl-right-inline">Rocío González Díaz. 2000.
Cohomology operations: A combinatorial approach. PhD thesis. University
of Seville. Retrieved from
<https://personal.us.es/rogodi/research/tesing01.pdf></span>

</div>

<div id="ref-chata:small-models" class="csl-entry">

<span class="csl-left-margin">\[70\]
</span><span class="csl-right-inline">Alvarez V., Armario J. A., Frau M.
D., Gonzalez-Diaz R., Jiménez M. J., Real P., and Silva B. 2000.
Computing “small” 1-homological models for commutative differential
graded algebras. In *Computer algebra in scientific computing
(Samarkand, 2000)*. Springer, Berlin, 87–100.
<https://doi.org/10.1007/978-3-642-57201-2_9></span>

</div>

<div id="ref-real:hpt" class="csl-entry">

<span class="csl-left-margin">\[71\]
</span><span class="csl-right-inline">Pedro Real. 2000. Homological
perturbation theory and associativity. *Homology, Homotopy and
Applications* 2, (2000), 51–88.
<https://doi.org/10.4310/hha.2000.v2.n1.a5></span>

</div>

<div id="ref-gr:steenrod-squares" class="csl-entry">

<span class="csl-left-margin">\[72\]
</span><span class="csl-right-inline">Rocío González-Díaz and Pedro
Real. 1999. A combinatorial method for computing Steenrod squares. In
*J. Pure Appl. Algebra*. 89–108.
<https://doi.org/10.1016/S0022-4049(99)00006-7></span>

</div>

<div id="ref-at-reductions" class="csl-entry">

<span class="csl-left-margin">\[73\]
</span><span class="csl-right-inline">P. R. Hurado, V. Álvarez, J. A.
Armario, and R. González-Díaz. 1999. Algorithms in algebraic topology
and homological algebra: The problem of the complexity. *Zapiski
Nauchnykh Seminarov POMI* 258, (1999), 161–184, 358.
<https://doi.org/10.1023/A:1013544506151></span>

</div>

<div id="ref-dousson:thesis" class="csl-entry">

<span class="csl-left-margin">\[74\]
</span><span class="csl-right-inline">Xavier Dousson. 1999. Homologie
effective des classifiants et calculs de groupes d’homotopie. PhD
thesis. l’Université Joseph Fourier. Retrieved from
<https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/Dousson-Xavier.pdf></span>

</div>

<div id="ref-kendoc" class="csl-entry">

<span class="csl-left-margin">\[75\]
</span><span class="csl-right-inline">Julio Rubio Garcia, Francis
Sergeraert, and Yvon Siret. 1999. *Kenzo: A symbolic software for
effective homology computation*. Institut Fourier, Grenoble, France.
Retrieved from
<https://github.com/miguelmarco/kenzo/tree/master/doc/doc_src></span>

</div>

<div id="ref-goerss-jardine" class="csl-entry">

<span class="csl-left-margin">\[76\]
</span><span class="csl-right-inline">Paul G. Goerss and John F.
Jardine. 1999. *Simplicial homotopy theory*. Birkhäuser Verlag, Basel.
<https://doi.org/10.1007/978-3-0348-8707-6></span>

</div>

<div id="ref-ks:iterating-bar" class="csl-entry">

<span class="csl-left-margin">\[77\]
</span><span class="csl-right-inline">T. Kadeishvili and S. Saneblidze.
1998. Iterating the bar construction. *Georgian Mathematical Journal* 5,
5 (1998), 441–452.
<https://doi.org/10.1023/B:GEOR.0000008115.37751.62></span>

</div>

<div id="ref-forman:morse" class="csl-entry">

<span class="csl-left-margin">\[78\]
</span><span class="csl-right-inline">Robin Forman. 1998. Morse theory
for cell complexes. *Advances in Mathematics* 134, 1 (1998), 90–145.
<https://doi.org/10.1006/aima.1997.1650></span>

</div>

<div id="ref-real:homotopy-groups" class="csl-entry">

<span class="csl-left-margin">\[79\]
</span><span class="csl-right-inline">Pedro Real. 1996. An algorithm
computing homotopy groups. *Math. Comput. Simulation* 42, 4-6 (1996),
461–465. <https://doi.org/10.1016/S0378-4754(96)00021-3></span>

</div>

<div id="ref-real:steenrod-squares" class="csl-entry">

<span class="csl-left-margin">\[80\]
</span><span class="csl-right-inline">Pedro Real. 1996. On the
computability of the Steenrod squares. *Ann. Univ. Ferrara Sez. VII
(N.S.)* 42, (1996), 57–63 (1998).
<https://doi.org/10.1007/BF02955020></span>

</div>

<div id="ref-morace-proute:twisting" class="csl-entry">

<span class="csl-left-margin">\[81\]
</span><span class="csl-right-inline">Frédéric Morace and Alain Prouté.
1994. Brown’s natural twisting cochain and the Eilenberg-Mac Lane
transformation. *J. Pure Appl. Algebra* 97, 1 (1994), 81–89.
<https://doi.org/10.1016/0022-4049(94)90040-X></span>

</div>

<div id="ref-morace:thesis" class="csl-entry">

<span class="csl-left-margin">\[82\]
</span><span class="csl-right-inline">Frédéric Morace. 1994. Cochaînes
de brown et transformation d’Eilenberg-Mac Lane: Réécriture en dimension
deux et homologie. PhD thesis. Paris 7. Retrieved from
<http://www.theses.fr/1994PA077273></span>

</div>

<div id="ref-real:thesis" class="csl-entry">

<span class="csl-left-margin">\[83\]
</span><span class="csl-right-inline">Pedro Real Jurado. 1993.
Algoritmos de cálculo de homología efectiva de los espacios
clasificantes. PhD thesis. Universidad de Sevilla, Departamento de
Geometría y Topología. Retrieved from
<https://idus.us.es/handle/11441/15908></span>

</div>

<div id="ref-rs:locally-effective" class="csl-entry">

<span class="csl-left-margin">\[84\]
</span><span class="csl-right-inline">J. Rubio and F. Sergeraert. 1993.
Locally effective objects and algebraic topology. In *Computational
algebraic geometry (Nice, 1992)*, Frédéric Eyssette and André Galligo
(eds.). Birkhäuser Boston, Boston, MA, 235–251.
<https://doi.org/10.1007/978-1-4612-2752-6_17></span>

</div>

<div id="ref-gl:perturbation-theory-ii" class="csl-entry">

<span class="csl-left-margin">\[85\]
</span><span class="csl-right-inline">V. K. A. M. Gugenheim, L. A.
Lambe, and J. D. Stasheff. 1991. Perturbation theory in differential
homological algebra. II. *Illinois J. Math.* 35, 3 (1991), 357–373.
<https://doi.org/10.1215/ijm/1255987784></span>

</div>

<div id="ref-hk:small-models-algebras" class="csl-entry">

<span class="csl-left-margin">\[86\]
</span><span class="csl-right-inline">Johannes Huebschmann and Tornike
Kadeishvili. 1991. Small models for chain algebras. *Math. Z.* 207, 2
(1991), 245–280. <https://doi.org/10.1007/BF02571387></span>

</div>

<div id="ref-gl:perturbation-theory-i" class="csl-entry">

<span class="csl-left-margin">\[87\]
</span><span class="csl-right-inline">V. K. A. M. Gugenheim and L. A.
Lambe. 1989. Perturbation theory in differential homological algebra. I.
*Illinois J. Math.* 33, 4 (1989), 566–582.
<https://doi.org/10.1215/ijm/1255988571></span>

</div>

<div id="ref-lambe-stasheff:perturbation" class="csl-entry">

<span class="csl-left-margin">\[88\]
</span><span class="csl-right-inline">Larry Lambe and Jim Stasheff.
1987. Applications of perturbation theory to iterated fibrations.
*Manuscripta Mathematica* 58, 3 (1987), 363–376.
<https://doi.org/10.1007/BF01165893></span>

</div>

<div id="ref-sergeraert:effective-1" class="csl-entry">

<span class="csl-left-margin">\[89\]
</span><span class="csl-right-inline">Francis Sergeraert. 1987.
Homologie effective. I. *Comptes Rendus de l’Académie des Sciences -
Series I - Mathematics* 304, 11 (1987), 279–282.</span>

</div>

<div id="ref-sergeraert:effective-2" class="csl-entry">

<span class="csl-left-margin">\[90\]
</span><span class="csl-right-inline">Francis Sergeraert. 1987.
Homologie effective. II. *Comptes Rendus de l’Académie des Sciences -
Series I - Mathematics* 304, 12 (1987), 319–321.</span>

</div>

<div id="ref-eml:homology-kgn-ii" class="csl-entry">

<span class="csl-left-margin">\[91\]
</span><span class="csl-right-inline">Samuel Eilenberg and Saunders Mac
Lane. 1954. On the groups $`H(\Pi,n)`$. II. Methods of computation.
*Ann. of Math. (2)* 60, (1954), 49–139.
<https://doi.org/10.2307/1969702></span>

</div>

<div id="ref-eml:homology-kgn-i" class="csl-entry">

<span class="csl-left-margin">\[92\]
</span><span class="csl-right-inline">Samuel Eilenberg and Saunders Mac
Lane. 1953. On the groups $`H(\Pi,n)`$. I. *Ann. of Math. (2)* 58,
(1953), 55–106. <https://doi.org/10.2307/1969820></span>

</div>

</div>
