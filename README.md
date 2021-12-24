AT
==

A Haskell rewrite of Kenzo.

Writing it from scratch myself is the only chance I have of
understanding it!

Central Definition
------------------

A *strong chain equivalence* between two chain complexes `C` and `D` is a
third chain complex `E` and two deformation retracts `l : E -> C` and
`r : E -> D`.

Let `X` be a simplicial set.

The *normalised chain complex* `N(X;Z)` has each `N(X;Z)_n` given by
the free abelian group on the set of nondegenerate `n` simplices of
`X`, with the boundary of a simplex `σ` calculated similar to usual,
but ignoring any degenerate faces.

An *effective homology structure* on `X` is a strong chain equivalence
between `N(X;Z)` and a chain complex `F`, where `F` is levelwise
finite dimensional.

A *simplicial set with effective homology* is a simplicial set `X`
equipped with an effective homology structure.

The point of Kenzo is that there are many constructions on simplicial
sets that extend to effective homology structures. And so if we begin
with a finite simplicial complex (equipped with the identity effective
homology structure) and perform complicated operations on it, then we
can compute the homology of the result, even if the actual simplices
are now far too complicated to get a handle on.

It seems harder to pin down what being merely 'locally effective'
means, something to do with at least having an algorithm that
identifies simplices, but not necessarily having effective homology.


Plan
----
Critial path to computing some non-trivial homotopy groups:
(Read bottom to top)
1. Whitehead tower
2. Pullbacks of fibrations
2. Effective fibrations
2. Cohomology classes, first non-trivial
2. Enough K(G,1)s
2. Classifying space of simplicial group
3. Bar construction on chain complexes
4. Tensor product of chain complexes <!-- 3. Product of simplicial sets, using the DVF method -->
5. Effective homology/Reductions/Equivalences

Wishlist
--------
- [ ] Algebra
  - [ ] Freyd Category?
  - [x] Chain Complex
    - [ ] Tensor
    - [ ] Bar
    - [ ] Cobar
  - [ ] Reduction
  - [ ] Perturbation
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
  - [ ] Products
  - [ ] Total Space of a Fibration
  - [ ] Loop Space
    - [ ] Group Structure
  - [x] Classifying Space
    - [x] For 0-reduced Group
    - [x] For Non-reduced Group?
    - [x] Special Case for Discrete Groups?
    - [ ] Group Structure
  - [ ] Specific Eilenberg-Maclane Spaces
    - [ ] K(Z,1)
    - [ ] K(Z/2,1) (Can be made particularly efficient)
    - [ ] K(Z/p,1)
    - [ ] Reduction of K(Z,1) to S^1
  - [ ] Suspension
- [ ] Effective Homology
  - [ ] Finite Simplicial Sets
  - [ ] Loop Space
  - [ ] Classifying Spaces
  - [ ] Total Space of a Fibration
  - [ ] Suspension
- [ ] Discrete Vector Field Algorithms
  - [ ] Classifying Spaces
  - [ ] Products
  - [ ] Fibrations
- [ ] Whitehead Tower

References
----------

### Code:
* [Kenzo homepage](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/)
* [Kenzo documenation](https://www-fourier.ujf-grenoble.fr/~sergerar/Kenzo/Kenzo-doc.pdf), likely out of date with the code
* ['Official' Kenzo mirror on Github](https://github.com/gheber/kenzo), there are three different
  versions of the code [here](https://github.com/gheber/kenzo/tree/master/src), I am not clear on the what
  gained/lost between them. There are some online [Jypter
  notebooks](https://sur-l-analysis-sit.us/) that let you play with Kenzo (which version?) without
  having to figure out how to install and operate a Common Lisp
  environment
* [Fork by Ana Romero + collaborators](https://github.com/miguelmarco/kenzo), has some added features
  over the mirror above, worth looking at [resolutions.lisp](https://github.com/miguelmarco/kenzo/blob/master/src/anromero/resolutions.lisp) and
  [homotopy.lisp](https://github.com/miguelmarco/kenzo/blob/master/src/anromero/homotopy.lisp), but is missing all the discrete vector field code
* [Modules written by Ana Romero](https://github.com/ana-romero/Kenzo-external-modules), mostly to do with spectral
  sequences

### Papers:
(Everything related to Kenzo I can find:)
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

<div id="ref-rs:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[5\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [A Bousfield-Kan
algorithm for computing the *effective* homotopy of a
space](https://doi.org/10.1007/s10208-016-9322-z), Found. Comput. Math.
17 (2017) 1335–1366.</span>

</div>

<div id="ref-rs:iterated-loop" class="csl-entry">

<span class="csl-left-margin">\[6\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [A combinatorial tool
for computing the effective homotopy of iterated loop
spaces](https://doi.org/10.1007/s00454-014-9650-1), Discrete Comput.
Geom. 53 (2015) 1–15.</span>

</div>

<div id="ref-rr:homology-of-groups" class="csl-entry">

<span class="csl-left-margin">\[7\] </span><span
class="csl-right-inline">A. Romero, J. Rubio, [Computing the homology of
groups: The geometric way](https://doi.org/10.1016/j.jsc.2011.12.007),
J. Symbolic Comput. 47 (2012) 752–770.</span>

</div>

<div id="ref-rs:homotopy-fibrations" class="csl-entry">

<span class="csl-left-margin">\[8\] </span><span
class="csl-right-inline">A. Romero, F. Sergeraert, [Effective homotopy
of fibrations](https://doi.org/10.1007/s00200-012-0168-6), Appl. Algebra
Engrg. Comm. Comput. 23 (2012) 85–100.</span>

</div>

<div id="ref-brs:a-infty" class="csl-entry">

<span class="csl-left-margin">\[9\] </span><span
class="csl-right-inline">A. Berciano Alcaraz, J. Rubio, F. Sergeraert, A
case study of *A*<sub>∞</sub>-structure, Georgian Math. J. 17 (2010)
57–77.</span>

</div>

<div id="ref-romero:bousfield-kan" class="csl-entry">

<span class="csl-left-margin">\[10\] </span><span
class="csl-right-inline">A. Romero, [Computing the first stages of the
Bousfield-Kan spectral
sequence](https://doi.org/10.1007/s00200-010-0123-3), Appl. Algebra
Engrg. Comm. Comput. 21 (2010) 227–248.</span>

</div>

<div id="ref-rrs:computing-spectral-sequences" class="csl-entry">

<span class="csl-left-margin">\[11\] </span><span
class="csl-right-inline">A. Romero, J. Rubio, F. Sergeraert, [Computing
spectral sequences](https://doi.org/10.1016/j.jsc.2006.06.002), J.
Symbolic Comput. 41 (2006) 1059–1079.</span>

</div>

<div id="ref-rs:locally-effective" class="csl-entry">

<span class="csl-left-margin">\[12\] </span><span
class="csl-right-inline">J. Rubio, F. Sergeraert, [Locally effective
objects and algebraic topology](https://doi.org/10.1007/978-1-4612-2752-6_17), in: Computational Algebraic
Geometry (Nice, 1992), Birkhäuser Boston, Boston, MA, 1993:
pp. 235–251.</span>

</div>

<div id="ref-sergeraert:effective-1" class="csl-entry">

<span class="csl-left-margin">\[13\] </span><span
class="csl-right-inline">F. Sergeraert, Homologie effective. I, C. R.
Acad. Sci. Paris Sér. I Math. 304 (1987) 279–282.</span>

</div>

<div id="ref-sergeraert:effective-2" class="csl-entry">

<span class="csl-left-margin">\[14\] </span><span
class="csl-right-inline">F. Sergeraert, Homologie effective. II, C. R.
Acad. Sci. Paris Sér. I Math. 304 (1987) 319–321.</span>

</div>

</div>

<!-- Types: -->
<!-- * `Math.ValueCategory`: Categories given by a type of objects and a type of morphisms -->
<!-- * `Math.ValueCategory.Arrow`: The arrow category of a given category -->
<!-- * `Math.ValueCategory.Sequential`: Sequences of objects indexed by the natural numbers -->
<!-- * `Math.ValueCategory.Abelian`: Categories whose homsets are abelian groups, and whose morphisms have well-behaved kernels and cokernels. -->
<!-- * `Math.ValueCategory.Effective`: Objects in an abelian category presented as the colimit of a sequence. -->

<!-- * `Math.Algebra.AbGroup`: F.g. abelian groups and their abelian category structure. -->

<!-- Main reference: -->
<!-- * Schön, Rolf. Effective Algebraic Topology. Vol. 451. American Mathematical Soc., 1991. -->

<!-- Operations on fg abelian groups: -->
<!-- * Barakat, Mohamed, and Daniel Robertz. "homalg – a meta-package for homological algebra." Journal of Algebra and its Applications 7.03 (2008): 299-317. -->
<!-- * Cohen, Cyril, and Anders Mörtberg. "A Coq formalization of finitely presented modules." International Conference on Interactive Theorem Proving. Springer International Publishing, 2014. -->
