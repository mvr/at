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
  - [ ] Cobar
    - [ ] Of 1-reduced
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
    - [ ] Treat `S¹` separately (not 1-reduced)
  - [x] Moore Spaces
  - [ ] Projective Spaces
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
  - [ ] [Of 0-reduced Abelian sGrps](#ref-sergeraert%3Advf-slides)
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
  - [x] `K(ℤ/n,1)`?
  - [x] Classifying Spaces for 0-reduced sAb
- Homotopy Groups
  - [ ] [Whitehead Tower (for 1-reduced sSet)](#ref-real%3Ahomotopy-groups)
  - [ ] [Postnikov Tower?](#ref-ckmvw%3Apoly-homotopy-groups)
- Cohomology Operations
  - [ ] [Over fields using  "minimal models"](#ref-gr%3Acohomology-ops) and

#### Misc TODOs
- [ ] Fix space leaks, jeez
- [ ] Pretty-printing for everything (unicode sub/superscripts in output?)
- [ ] Docs for everything
- [ ] Move this list to Github issues
- [ ] Consolidate some files? Eg. Sum, Shift into ChainComplex
- [ ] Use bit operations eg from
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
- [ ] Add methods to produce representatives of homology classes

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
Everything relevant to effective algebraic topology that I can find
(not all of which is relevant for implementation). Some of the
documents have multiple versions; I have tried to link to the most
recent in each case. Some material is repeated in different
references.

<!-- To generate: pandoc kenzo.bib -C --csl=acm-sig-proceedings.csl -t gfm -o out.md -->
<div id="refs" class="references csl-bib-body">

<div id="ref-franz:twisting" class="csl-entry">

<span class="csl-left-margin">\[1\] </span><span
class="csl-right-inline">Franz, M. 2021. [Szczarba’s twisting cochain
and the Eilenberg-Zilber
maps](https://doi.org/10.1007/s13348-020-00299-x). *Collectanea
Mathematica*. 72, 3 (2021), 569–586.</span>

</div>

<div id="ref-gr:leray-serre" class="csl-entry">

<span class="csl-left-margin">\[2\] </span><span
class="csl-right-inline">Guidolin, A. and Romero, A. 2021. [Computing
higher Leray-Serre spectral sequences of towers of
fibrations](https://doi.org/10.1007/s10208-020-09475-8). *Foundations of
Computational Mathematics*. 21, 4 (2021), 1023–1074.</span>

</div>

<div id="ref-rrss:em-spectral-sequence" class="csl-entry">

<span class="csl-left-margin">\[3\] </span><span
class="csl-right-inline">Romero, A., Rubio, J., Sergeraert, F. and
Szymik, M. 2020. [A new Kenzo module for computing the Eilenberg-Moore
spectral sequence](https://doi.org/10.1145/3427218.3427225). *ACM
Communications in Computer Algebra*. 54, 2 (2020), 57–60.</span>

</div>

<div id="ref-rrs:fibrations-implementation" class="csl-entry">

<span class="csl-left-margin">\[4\] </span><span
class="csl-right-inline">Romero, A., Rubio, J. and Sergeraert, F. 2019.
[An implementation of effective homotopy of
fibrations](https://doi.org/10.1016/j.jsc.2018.08.001). *Journal of
Symbolic Computation*. 94, (2019), 149–172.</span>

</div>

<div id="ref-as:ez-dvf" class="csl-entry">

<span class="csl-left-margin">\[5\] </span><span
class="csl-right-inline">Romero, A. and Sergeraert, F. 2019. [The
Eilenberg-Zilber theorem via discrete vector
fields](https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/EZ-submitted.pdf).</span>

</div>

<div id="ref-zhechev:thesis" class="csl-entry">

<span class="csl-left-margin">\[6\] </span><span
class="csl-right-inline">Zhechev, S. 2019. *[Algorithmic aspects of
homotopy theory and
embeddability](https://doi.org/10.15479/AT:ISTA:6681)*. Institute of
Science; Technology Austria.</span>

</div>

<div id="ref-sergeraert:hexagonal-lemma" class="csl-entry">

<span class="csl-left-margin">\[7\] </span><span
class="csl-right-inline">Sergeraert, F. 2018. [The homological hexagonal
lemma](https://doi.org/10.1515/gmj-2018-0055). *Georgian Mathematical
Journal*. 25, 4 (2018), 603–622.</span>

</div>

<div id="ref-vj:ainfty-algorithms" class="csl-entry">

<span class="csl-left-margin">\[8\] </span><span
class="csl-right-inline">Vejdemo-Johansson, M. 2018. [Algorithms in
*A*<sup>∞</sup>-algebras](https://doi.org/10.1515/gmj-2018-0057).
*Georgian Mathematical Journal*. 25, 4 (2018), 629–635.</span>

</div>

<div id="ref-rs:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[9\] </span><span
class="csl-right-inline">Romero, A. and Sergeraert, F. 2017. [A
Bousfield-Kan algorithm for computing the *effective* homotopy of a
space](https://doi.org/10.1007/s10208-016-9322-z). *Foundations of
Computational Mathematics*. 17, 5 (2017), 1335–1366.</span>

</div>

<div id="ref-hess:twisting-cochain" class="csl-entry">

<span class="csl-left-margin">\[10\] </span><span
class="csl-right-inline">Hess, K. 2016. [The Hochschild complex of a
twisting cochain](https://doi.org/10.1016/j.jalgebra.2015.11.040).
*Journal of Algebra*. 451, (2016), 302–356.</span>

</div>

<div id="ref-rs:iterated-loop" class="csl-entry">

<span class="csl-left-margin">\[11\] </span><span
class="csl-right-inline">Romero, A. and Sergeraert, F. 2015. [A
combinatorial tool for computing the effective homotopy of iterated loop
spaces](https://doi.org/10.1007/s00454-014-9650-1). *Discrete &
Computational Geometry*. 53, 1 (2015), 1–15.</span>

</div>

<div id="ref-ckmsvw:maps-into-sphere" class="csl-entry">

<span class="csl-left-margin">\[12\] </span><span
class="csl-right-inline">Čadek, M., Krčál, M., Matoušek, J., Sergeraert,
F., Vokřínek, L. and Wagner, U. 2014. [Computing all maps into a
sphere](https://doi.org/10.1145/2597629). *Journal of the ACM*. 61, 3
(2014), Art. 17, 44.</span>

</div>

<div id="ref-ckmvw:poly-homotopy-groups" class="csl-entry">

<span class="csl-left-margin">\[13\] </span><span
class="csl-right-inline">Čadek, M., Krčál, M., Matoušek, J., Vokřínek,
L. and Wagner, U. 2014. [Polynomial-time computation of homotopy groups
and Postnikov systems in fixed
dimension](https://doi.org/10.1137/120899029). *SIAM Journal on
Computing*. 43, 5 (2014), 1728–1780.</span>

</div>

<div id="ref-filakovsky:hocolim" class="csl-entry">

<span class="csl-left-margin">\[14\] </span><span
class="csl-right-inline">Filakovský, M. 2014. [Effective homology for
homotopy colimit and cofibrant
replacement](https://doi.org/10.5817/AM2014-5-273). *Universitatis
Masarykianae Brunensis. Facultas Scientiarum Naturalium. Archivum
Mathematicum*. 50, 5 (2014), 273–286.</span>

</div>

<div id="ref-sergeraert:dvf-slides" class="csl-entry">

<span class="csl-left-margin">\[15\] </span><span
class="csl-right-inline">Sergeraert, F. 2013. [Discrete vector fields
and fundamental algebraic
topology](https://www-fourier.ujf-grenoble.fr/~sergerar/Talks/13-04-Tokyo.pdf).</span>

</div>

<div id="ref-kms:poly-em-spaces" class="csl-entry">

<span class="csl-left-margin">\[16\] </span><span
class="csl-right-inline">Krčál, M., Matoušek, J. and Sergeraert, F.
2013. [Polynomial-time homology for simplicial Eilenberg-MacLane
spaces](https://doi.org/10.1007/s10208-013-9159-7). *Foundations of
Computational Mathematics. The Journal of the Society for the
Foundations of Computational Mathematics*. 13, 6 (2013), 935–963.</span>

</div>

<div id="ref-filakovsky:twisted-products" class="csl-entry">

<span class="csl-left-margin">\[17\] </span><span
class="csl-right-inline">Filakovský, M. 2012. [Effective chain complexes
for twisted products](https://doi.org/10.5817/AM2012-5-313).
*Universitatis Masarykianae Brunensis. Facultas Scientiarum Naturalium.
Archivum Mathematicum*. 48, 5 (2012), 313–322.</span>

</div>

<div id="ref-rr:homology-of-groups" class="csl-entry">

<span class="csl-left-margin">\[18\] </span><span
class="csl-right-inline">Romero, A. and Rubio, J. 2012. [Computing the
homology of groups: The geometric
way](https://doi.org/10.1016/j.jsc.2011.12.007). *Journal of Symbolic
Computation*. 47, 7 (2012), 752–770.</span>

</div>

<div id="ref-as:dvf" class="csl-entry">

<span class="csl-left-margin">\[19\] </span><span
class="csl-right-inline">Romero, A. and Sergeraert, F. 2012. [Discrete
vector fields and fundamental algebraic
topology](https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Vector-Fields.pdf).</span>

</div>

<div id="ref-rs:homotopy-fibrations" class="csl-entry">

<span class="csl-left-margin">\[20\] </span><span
class="csl-right-inline">Romero, A. and Sergeraert, F. 2012. [Effective
homotopy of fibrations](https://doi.org/10.1007/s00200-012-0168-6).
*Applicable Algebra in Engineering, Communication and Computing*. 23,
1-2 (2012), 85–100.</span>

</div>

<div id="ref-rs:constructive-homology" class="csl-entry">

<span class="csl-left-margin">\[21\] </span><span
class="csl-right-inline">Rubio, J. and Sergeraert, F. 2012.
[Constructive homological algebra and
applications](https://arxiv.org/abs/1208.3816).</span>

</div>

<div id="ref-stevenson:decalage" class="csl-entry">

<span class="csl-left-margin">\[22\] </span><span
class="csl-right-inline">Stevenson, D. 2012. [Décalage and Kan’s
simplicial loop group
functor](http://www.tac.mta.ca/tac/volumes/26/28/26-28abs.html). *Theory
and Applications of Categories*. 26, (2012), No. 28, 768–787.</span>

</div>

<div id="ref-spiwack:thesis" class="csl-entry">

<span class="csl-left-margin">\[23\] </span><span
class="csl-right-inline">Spiwack, A. 2011. *[<span
class="nocase">Verified Computing in Homological
Algebra</span>](https://pastel.archives-ouvertes.fr/pastel-00605836)*.
Ecole Polytechnique X.</span>

</div>

<div id="ref-heras:pushout" class="csl-entry">

<span class="csl-left-margin">\[24\] </span><span
class="csl-right-inline">Heras, J. 2010. [Pushout construction for the
Kenzo
systems](https://www.unirioja.es/cu/joheras/pushout/Doc/pushout.pdf).</span>

</div>

<div id="ref-real:twisted-ez" class="csl-entry">

<span class="csl-left-margin">\[25\] </span><span
class="csl-right-inline">Álvarez, V., Armario, J.A., Frau, M.D. and
Real, P. 2010. [Cartan’s constructions and the twisted Eilenberg-Zilber
theorem](https://doi.org/10.1515/gmj.2010.006). *Georgian Mathematical
Journal*. 17, 1 (2010), 13–23.</span>

</div>

<div id="ref-brs:a-infty" class="csl-entry">

<span class="csl-left-margin">\[26\] </span><span
class="csl-right-inline">Berciano Alcaraz, A., Rubio, J. and Sergeraert,
F. 2010. A case study of *A*<sub>∞</sub>-structure. *Georgian
Mathematical Journal*. 17, 1 (2010), 57–77.</span>

</div>

<div id="ref-heras:pushout-conf" class="csl-entry">

<span class="csl-left-margin">\[27\] </span><span
class="csl-right-inline">Heras, J. 2010. [Effective homology of the
pushout of simplicial sets](https://arxiv.org/abs/1410.3651).
*Proceedings of the XII encuentros de álgebra computacional y
aplicaciones* (2010), 152–156.</span>

</div>

<div id="ref-hprr:integrating-sources" class="csl-entry">

<span class="csl-left-margin">\[28\] </span><span
class="csl-right-inline">Heras, J., Pascual, V., Romero, A. and Rubio,
J. 2010. [Integrating multiple sources to answer questions in algebraic
topology](https://arxiv.org/abs/1005.0749). *Proceedings of the 10th
ASIC and 9th MKM international conference, and 17th calculemus
conference on intelligent computer mathematics* (Paris, France, 2010),
331–335.</span>

</div>

<div id="ref-hess-tonks:loop-group" class="csl-entry">

<span class="csl-left-margin">\[29\] </span><span
class="csl-right-inline">Hess, K. and Tonks, A. 2010. [The loop group
and the cobar
construction](https://doi.org/10.1090/S0002-9939-09-10238-1).
*Proceedings of the American Mathematical Society*. 138, 5 (2010),
1861–1876.</span>

</div>

<div id="ref-romero:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[30\] </span><span
class="csl-right-inline">Romero, A. 2010. [Computing the first stages of
the Bousfield-Kan spectral
sequence](https://doi.org/10.1007/s00200-010-0123-3). *Applicable
Algebra in Engineering, Communication and Computing*. 21, 3 (2010),
227–248.</span>

</div>

<div id="ref-real:algebra-structures" class="csl-entry">

<span class="csl-left-margin">\[31\] </span><span
class="csl-right-inline">Álvarez, V., Armario, J.A., Frau, M.D. and
Real, P. 2009. [Algebra structures on the comparison of the reduced bar
construction and the reduced
*W*-construction](https://doi.org/10.1080/00927870902747662).
*Communications in Algebra*. 37, 10 (2009), 3643–3665.</span>

</div>

<div id="ref-rer:classifying-space" class="csl-entry">

<span class="csl-left-margin">\[32\] </span><span
class="csl-right-inline">Romero, A., Ellis, G. and Rubio, J. 2009.
[Interoperating between computer algebra systems: Computing homology of
groups with Kenzo and GAP](https://doi.org/10.1145/1576702.1576744).
*ISSAC 2009—Proceedings of the 2009 International Symposium on Symbolic
and Algebraic Computation* (2009), 303–310.</span>

</div>

<div id="ref-sergeraert:cp-spaces" class="csl-entry">

<span class="csl-left-margin">\[33\] </span><span
class="csl-right-inline">Sergeraert, F. 2009. [Triangulations of complex
projective
spaces](https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Mirian.pdf).</span>

</div>

<div id="ref-thomas:wbar" class="csl-entry">

<span class="csl-left-margin">\[34\] </span><span
class="csl-right-inline">Thomas, S. 2008. [The functors *W̄* and
Diag ∘ Nerve are simplicially homotopy
equivalent](https://arxiv.org/abs/0804.1082). *Journal of Homotopy and
Related Structures*. 3, 1 (2008), 359–378.</span>

</div>

<div id="ref-real:reducing-costs" class="csl-entry">

<span class="csl-left-margin">\[35\] </span><span
class="csl-right-inline">Berciano, A., Jiménez, M.J. and Real, P. 2006.
[Reducing computational costs in the basic perturbation
lemma](https://doi.org/10.1007/11870814_3). *Computer algebra in
scientific computing*. V.G. Ganzha, E.W. Mayr, and E.V. Vorozhtsov, eds.
Springer, Berlin. 33–48.</span>

</div>

<div id="ref-rrs:computing-spectral-sequences" class="csl-entry">

<span class="csl-left-margin">\[36\] </span><span
class="csl-right-inline">Romero, A., Rubio, J. and Sergeraert, F. 2006.
[Computing spectral
sequences](https://doi.org/10.1016/j.jsc.2006.06.002). *Journal of
Symbolic Computation*. 41, 10 (2006), 1059–1079.</span>

</div>

<div id="ref-gr:cohomology-ops" class="csl-entry">

<span class="csl-left-margin">\[37\] </span><span
class="csl-right-inline">González-Díaz, R. and Real, P. 2003.
[Computation of cohomology operations of finite simplicial
complexes](https://doi.org/10.4310/HHA.2003.v5.n2.a4). *Homology
Homotopy Appl.* 83–93.</span>

</div>

<div id="ref-clement:thesis" class="csl-entry">

<span class="csl-left-margin">\[38\] </span><span
class="csl-right-inline">Clément, A. 2002. *[Integral cohomology of
finite Postnikov towers](https://doc.rero.ch/record/482)*. Université de
Lausanne.</span>

</div>

<div id="ref-real:hpt" class="csl-entry">

<span class="csl-left-margin">\[39\] </span><span
class="csl-right-inline">Real, P. 2000. [Homological perturbation theory
and associativity](https://doi.org/10.4310/hha.2000.v2.n1.a5).
*Homology, Homotopy and Applications*. 2, (2000), 51–88.</span>

</div>

<div id="ref-dousson:thesis" class="csl-entry">

<span class="csl-left-margin">\[40\] </span><span
class="csl-right-inline">Dousson, X. 1999. *[Homologie effective des
classifiants et calculs de groupes
d’homotopie](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/Dousson-Xavier.pdf)*.
l’Université Joseph Fourier.</span>

</div>

<div id="ref-goerss-jardine" class="csl-entry">

<span class="csl-left-margin">\[41\] </span><span
class="csl-right-inline">Goerss, P.G. and Jardine, J.F. 1999.
*[Simplicial homotopy
theory](https://doi.org/10.1007/978-3-0348-8707-6)*. Birkhäuser Verlag,
Basel.</span>

</div>

<div id="ref-at-reductions" class="csl-entry">

<span class="csl-left-margin">\[42\] </span><span
class="csl-right-inline">Hurado, P.R., Álvarez, V., Armario, J.A. and
González-Díaz, R. 1999. [Algorithms in algebraic topology and
homological algebra: The problem of the
complexity](https://doi.org/10.1023/A:1013544506151). *Zapiski Nauchnykh
Seminarov POMI*. 258, (1999), 161–184, 358.</span>

</div>

<div id="ref-kendoc" class="csl-entry">

<span class="csl-left-margin">\[43\] </span><span
class="csl-right-inline">Rubio Garcia, J., Sergeraert, F. and Siret, Y.
1999. *[Kenzo: A symbolic software for effective homology
computation](https://github.com/miguelmarco/kenzo/tree/master/doc/doc_src)*.
Institut Fourier.</span>

</div>

<div id="ref-forman:morse" class="csl-entry">

<span class="csl-left-margin">\[44\] </span><span
class="csl-right-inline">Forman, R. 1998. [Morse theory for cell
complexes](https://doi.org/10.1006/aima.1997.1650). *Advances in
Mathematics*. 134, 1 (1998), 90–145.</span>

</div>

<div id="ref-real:homotopy-groups" class="csl-entry">

<span class="csl-left-margin">\[45\] </span><span
class="csl-right-inline">Real, P. 1996. [An algorithm computing homotopy
groups](https://doi.org/10.1016/S0378-4754(96)00021-3). *Math. Comput.
Simulation*. 42, 4-6 (1996), 461–465.</span>

</div>

<div id="ref-real:steenrod-squares" class="csl-entry">

<span class="csl-left-margin">\[46\] </span><span
class="csl-right-inline">Real, P. 1996. [On the computability of the
Steenrod squares](https://doi.org/10.1007/BF02955020). *Ann. Univ.
Ferrara Sez. VII (N.S.)*. 42, (1996), 57–63 (1998).</span>

</div>

<div id="ref-morace:thesis" class="csl-entry">

<span class="csl-left-margin">\[47\] </span><span
class="csl-right-inline">Morace, F. 1994. *[Cochaînes de brown et
transformation d’Eilenberg-Mac Lane: Réécriture en dimension deux et
homologie](http://www.theses.fr/1994PA077273)*. Paris 7.</span>

</div>

<div id="ref-morace-proute:twisting" class="csl-entry">

<span class="csl-left-margin">\[48\] </span><span
class="csl-right-inline">Morace, F. and Prouté, A. 1994. [Brown’s
natural twisting cochain and the Eilenberg-Mac Lane
transformation](https://doi.org/10.1016/0022-4049(94)90040-X). *J. Pure
Appl. Algebra*. 97, 1 (1994), 81–89.</span>

</div>

<div id="ref-real:thesis" class="csl-entry">

<span class="csl-left-margin">\[49\] </span><span
class="csl-right-inline">Real Jurado, P. 1993. *[Algoritmos de cálculo
de homología efectiva de los espacios
clasificantes](https://idus.us.es/handle/11441/15908)*. Universidad de
Sevilla, Departamento de Geometría y Topología.</span>

</div>

<div id="ref-rs:locally-effective" class="csl-entry">

<span class="csl-left-margin">\[50\] </span><span
class="csl-right-inline">Rubio, J. and Sergeraert, F. 1993. [Locally
effective objects and algebraic
topology](https://doi.org/10.1007/978-1-4612-2752-6_17). *Computational
algebraic geometry (Nice, 1992)*. F. Eyssette and A. Galligo, eds.
Birkhäuser Boston, Boston, MA. 235–251.</span>

</div>

<div id="ref-lambe-stasheff:perturbation" class="csl-entry">

<span class="csl-left-margin">\[51\] </span><span
class="csl-right-inline">Lambe, L. and Stasheff, J. 1987. [Applications
of perturbation theory to iterated
fibrations](https://doi.org/10.1007/BF01165893). *Manuscripta
Mathematica*. 58, 3 (1987), 363–376.</span>

</div>

<div id="ref-sergeraert:effective-1" class="csl-entry">

<span class="csl-left-margin">\[52\] </span><span
class="csl-right-inline">Sergeraert, F. 1987. Homologie effective. I.
*Comptes Rendus de l’Académie des Sciences - Series I - Mathematics*.
304, 11 (1987), 279–282.</span>

</div>

<div id="ref-sergeraert:effective-2" class="csl-entry">

<span class="csl-left-margin">\[53\] </span><span
class="csl-right-inline">Sergeraert, F. 1987. Homologie effective. II.
*Comptes Rendus de l’Académie des Sciences - Series I - Mathematics*.
304, 12 (1987), 319–321.</span>

</div>

</div>
