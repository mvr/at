AT
==

A Haskell rewrite of Kenzo.

Writing it from scratch myself is the only chance I have of
understanding it!

Central Concepts of Kenzo
-------------------------

A *simplicial set* `X` is described by a type `a`, containing the data
required to specify `X`, and a type `GeomSimplex a`, whose elements
correspond to non-degenerate simplices (in Kenzo called 'geometric
simplices'). Like Kenzo we also allow a predicate on `GeomSimplex a`
specifying when an element actually describes a simplex and when it is
'spurious'.

An actual simplex of `X` is a geometric simplex together with a
'formal degeneracy', which is a list of degeneracy operators in normal
form. Face maps are implemented as functions from geometric simplices
to (possibly degenerate) simplices, and this function is easily to all
simplices in a way forced by the simplicial identities.

A simplicial set is *levelwise finite* if there is a finite number of
geometric simplices for each dimension, and a function giving a list
of these simplices for any `n`. It is not required that there are only
finitely many geometric simplices overall.

The *normalised chain complex* `N(X;Z)` of `X` has each `N(X;Z)_n`
given by the free abelian group on the set of nondegenerate `n`
simplices of `X`, with the boundary of a simplex `σ` calculated
similar to usual, but ignoring any degenerate faces.

The quotient map `C(X) -> N(X)` from the ordinary chain complex `C(X)`
is a quasi-isomorphism, and so if `X` is levelwise finite, then the
homology of `X` can be computed by using the finite dimensional
matrices describing the differentials of `N(X)`.

But many constructions on simplicial sets do not preserve levelwise
finiteness, and so we need some other way to calculate homology. This
is where 'effective homology' comes in.

A *strong chain equivalence* between two chain complexes `C` and `D` is a
third chain complex `E` and two deformation retracts `l : E -> C` and
`r : E -> D`.

An *effective homology structure* on `X` is a strong chain equivalence
between `N(X;Z)` and a levelwise finite dimensional chain complex `F`.

A *simplicial set with effective homology* is a simplicial set `X`
equipped with an effective homology structure.

The point of Kenzo is that although constructions on simplicial sets
often do not preserve effectiveness, they *do* extend to effective
homology structures. And so if we begin with a finite simplicial
complex and perform operations on it, then we can compute the homology
of the result, even if the actual simplices are now far too
complicated to get a handle on.

Notes
-----

- I have switched a little terminology: I believe Kenzo uses
  'effective' for levelwise finite things and 'locally effective' for
  what I am calling effective things, but I find this a bit confusing.
- In Kenzo, every sSet is automatically also the
- I am uncertain whether, in the definition of 'effective homology
  structure', it is necessary to use a chain equivalence. Maybe an
  uncollapsed zigzag of reductions is enough.
- The algorithms for some of the constructions are very similar to
  each other. There may be a way to unify them via bisimplicial sets
  and the ['generalised Eilenberg-Zilber
  theorm'](https://ncatlab.org/nlab/show/Eilenberg-Zilber+theorem)
  releating the diagonal and total complexes.
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
- classes.lisp in Kenzo contains the meaning of the inscrutible 4
  letter abbreviations
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
- Unguessable CL functions
  * (ash x n) = bit shift x left by n

Plan
----
Critial path to computing some non-trivial homotopy groups:
(Read bottom to top)
1. Whitehead tower
2. Cohomology classes
2. Effective classifying spaces via a DVF
3. Bar construction on chain complexes
2. Pullbacks of fibrations
2. Effective principal fibrations via a DVF
2. Effective products of sSets via DVF
4. Tensor product of chain complexes
5. Effective homology/reductions/equivalences

General TODOs:
- Consider using numeric-prelude to clean up the (1-)algebraic
  structures


Wishlist
--------
- [ ] Algebra
  - [ ] Freyd Category?
  - [x] Chain Complex
    - [x] Tensor
    - [ ] Bar
    - [ ] Cobar
  - [x] Reduction
  - [x] Perturbation
- [ ] Definitions
  - [x] Simplicial Set
  - [x] Simplicial Morphism
  - [x] Simplicial Group
  - [x] SSet With Effective Homology
  - [ ] Principal Fibrations
- [ ] Constructions
  - [x] Spheres
  - [x] Moore Spaces
  - [ ] Projective Spaces
  - [x] Products
  - [ ] Total Space of a Fibration
  - [ ] Loop Space
    - [ ] Group Structure
  - [ ] Suspension
  - [x] Classifying Space
    - [x] For 0-reduced Group
    - [x] For Non-reduced Group?
    - [x] Special Case for Discrete Groups?
    - [ ] Group Structure
  - [ ] Efficient Eilenberg-Maclane Spaces
    - [ ] `K(Z,1)`
    - [ ] `K(Z/2,1)` (Can be made particularly efficient)
    - [ ] `K(Z/p,1)`
    - [ ] Reduction of `K(Z,1)` to `S^1`
- [ ] Effective Homology
  - [x] Effective Simplicial Sets
  - [ ] Classifying Spaces
    - [ ] Of 1-Reduced Abelian sGrps
    - [ ] Of General sGrps
  - [x] Products
    - [ ] Use specialised `f` and `g` defined directly
  - [ ] Total Space of a Fibration
  - [ ] Loop Space
  - [ ] Suspension
- [x] Discrete Vector Field
  - [x] Products
  - [ ] Fibrations
  - [ ] Classifying Spaces for 1-Reduced Abelian Groups
- [ ] Whitehead Tower

References
----------

### Code:
* [Kenzo homepage](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/)
* [Kenzo documenation](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/Kenzo-doc.pdf), likely out of date with the code
* ['Official' Kenzo mirror on Github](https://github.com/gheber/kenzo), there are three different
  versions of the code [here](https://github.com/gheber/kenzo/tree/master/src), I am not clear on what is
  gained/lost between them. There are online [Jypter
  notebooks](https://sur-l-analysis-sit.us/) that let you play with Kenzo (which version?) without
  having to figure out how to install and operate a Common Lisp
  environment
* [Fork by Ana Romero + collaborators](https://github.com/miguelmarco/kenzo), has some added features
  over the mirror above, worth looking at [resolutions.lisp](https://github.com/miguelmarco/kenzo/blob/master/src/anromero/resolutions.lisp) and
  [homotopy.lisp](https://github.com/miguelmarco/kenzo/blob/master/src/anromero/homotopy.lisp), but is missing all the discrete vector field code
* [Modules written by Ana Romero](https://github.com/ana-romero/Kenzo-external-modules), mostly to do with spectral
  sequences

### Papers:
Everything related to Kenzo I can find. Some of the documents have
different versions, I have tried to link to the most recent in each
case.

<!-- To generate: pandoc kenzo.bib -C --csl=elsevier-with-titles.csl -t gfm -o out.md -->
<div id="refs" class="references csl-bib-body">

<div id="ref-gr:leray-serre" class="csl-entry">

<span class="csl-left-margin">\[1\] </span><span
class="csl-right-inline">A. Guidolin, A. Romero, [Computing higher
Leray-Serre spectral sequences of towers of
fibrations](https://doi.org/10.1007/s10208-020-09475-8), Found. Comput.
Math. 21 (2021) 1023–1074.</span>

</div>

<div id="ref-rrss:em-spectral-sequence" class="csl-entry">

<span class="csl-left-margin">\[2\] </span><span
class="csl-right-inline">A. Romero, J. Rubio, F. Sergeraert, M. Szymik,
[A new Kenzo module for computing the Eilenberg-Moore spectral
sequence](https://doi.org/10.1145/3427218.3427225), ACM Commun. Comput.
Algebra. 54 (2020) 57–60.</span>

</div>

<div id="ref-rrs:fibrations-implementation" class="csl-entry">

<span class="csl-left-margin">\[3\] </span><span
class="csl-right-inline">A. Romero, J. Rubio, F. Sergeraert, [An
implementation of effective homotopy of
fibrations](https://doi.org/10.1016/j.jsc.2018.08.001), J. Symbolic
Comput. 94 (2019) 149–172.</span>

</div>

<div id="ref-as:ez-dvf" class="csl-entry">

<span class="csl-left-margin">\[4\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [The eilenberg-zilber
theorem via discrete vector
fields](https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/EZ-submitted.pdf),
(2019).</span>

</div>

<div id="ref-sergeraert:hexagonal-lemma" class="csl-entry">

<span class="csl-left-margin">\[5\] </span><span
class="csl-right-inline">F. Sergeraert, [The homological hexagonal
lemma](https://doi.org/10.1515/gmj-2018-0055), Georgian Math. J. 25
(2018) 603–622.</span>

</div>

<div id="ref-rs:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[6\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [A Bousfield-Kan
algorithm for computing the *effective* homotopy of a
space](https://doi.org/10.1007/s10208-016-9322-z), Found. Comput. Math.
17 (2017) 1335–1366.</span>

</div>

<div id="ref-rs:iterated-loop" class="csl-entry">

<span class="csl-left-margin">\[7\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [A combinatorial tool
for computing the effective homotopy of iterated loop
spaces](https://doi.org/10.1007/s00454-014-9650-1), Discrete Comput.
Geom. 53 (2015) 1–15.</span>

</div>

<div id="ref-rr:homology-of-groups" class="csl-entry">

<span class="csl-left-margin">\[8\] </span><span
class="csl-right-inline">A. Romero, J. Rubio, [Computing the homology of
groups: The geometric way](https://doi.org/10.1016/j.jsc.2011.12.007),
J. Symbolic Comput. 47 (2012) 752–770.</span>

</div>

<div id="ref-rs:homotopy-fibrations" class="csl-entry">

<span class="csl-left-margin">\[9\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [Effective homotopy
of fibrations](https://doi.org/10.1007/s00200-012-0168-6), Appl. Algebra
Engrg. Comm. Comput. 23 (2012) 85–100.</span>

</div>

<div id="ref-rs:constructive-homology" class="csl-entry">

<span class="csl-left-margin">\[10\] </span><span
class="csl-right-inline">J. Rubio, F. Sergeraert, [Constructive
homological algebra and applications](https://arxiv.org/abs/1208.3816),
(2012).</span>

</div>

<div id="ref-brs:a-infty" class="csl-entry">

<span class="csl-left-margin">\[11\] </span><span
class="csl-right-inline">A. Berciano Alcaraz, J. Rubio, F. Sergeraert, A
case study of *A*<sub>∞</sub>-structure, Georgian Math. J. 17 (2010)
57–77.</span>

</div>

<div id="ref-romero:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[12\] </span><span
class="csl-right-inline">A. Romero, [Computing the first stages of the
Bousfield-Kan spectral
sequence](https://doi.org/10.1007/s00200-010-0123-3), Appl. Algebra
Engrg. Comm. Comput. 21 (2010) 227–248.</span>

</div>

<div id="ref-as:dvf" class="csl-entry">

<span class="csl-left-margin">\[13\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [Discrete vector
fields and fundamental algebraic
topology](https://arxiv.org/abs/1005.5685), (2010).</span>

</div>

<div id="ref-rer:classifying-space" class="csl-entry">

<span class="csl-left-margin">\[14\] </span><span
class="csl-right-inline">A. Romero, G. Ellis, J. Rubio, [Interoperating
between computer algebra systems: Computing homology of groups with
Kenzo and GAP](https://doi.org/10.1145/1576702.1576744), in: ISSAC
2009—Proceedings of the 2009 International Symposium on Symbolic and
Algebraic Computation, ACM, New York, 2009: pp. 303–310.</span>

</div>

<div id="ref-rrs:computing-spectral-sequences" class="csl-entry">

<span class="csl-left-margin">\[15\] </span><span
class="csl-right-inline">A. Romero, J. Rubio, F. Sergeraert, [Computing
spectral sequences](https://doi.org/10.1016/j.jsc.2006.06.002), J.
Symbolic Comput. 41 (2006) 1059–1079.</span>

</div>

<div id="ref-real:hpt" class="csl-entry">

<span class="csl-left-margin">\[16\] </span><span
class="csl-right-inline">P. Real, [Homological perturbation theory and
associativity](https://doi.org/10.4310/hha.2000.v2.n1.a5), Homology
Homotopy Appl. 2 (2000) 51–88.</span>

</div>

<div id="ref-forman:morse" class="csl-entry">

<span class="csl-left-margin">\[17\] </span><span
class="csl-right-inline">R. Forman, [Morse theory for cell
complexes](https://doi.org/10.1006/aima.1997.1650), Adv. Math. 134
(1998) 90–145.</span>

</div>

<div id="ref-morace1994cochaines" class="csl-entry">

<span class="csl-left-margin">\[18\] </span><span
class="csl-right-inline">F. Morace, [Cochaînes de brown et
transformation d’eilenberg-mac lane: Réécriture en dimension deux et
homologie](http://www.theses.fr/1994PA077273), PhD thesis, Paris 7,
1994.</span>

</div>

<div id="ref-rs:locally-effective" class="csl-entry">

<span class="csl-left-margin">\[19\] </span><span
class="csl-right-inline">J. Rubio, F. Sergeraert, [Locally effective
objects and algebraic
topology](https://doi.org/10.1007/978-1-4612-2752-6_17), in:
Computational Algebraic Geometry (Nice, 1992), Birkhäuser Boston,
Boston, MA, 1993: pp. 235–251.</span>

</div>

<div id="ref-sergeraert:effective-1" class="csl-entry">

<span class="csl-left-margin">\[20\] </span><span
class="csl-right-inline">F. Sergeraert, Homologie effective. I, C. R.
Acad. Sci. Paris Sér. I Math. 304 (1987) 279–282.</span>

</div>

<div id="ref-sergeraert:effective-2" class="csl-entry">

<span class="csl-left-margin">\[21\] </span><span
class="csl-right-inline">F. Sergeraert, Homologie effective. II, C. R.
Acad. Sci. Paris Sér. I Math. 304 (1987) 319–321.</span>

</div>

</div>
