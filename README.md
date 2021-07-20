
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phwelike

<!-- badges: start -->

[![NSF-2132247](https://img.shields.io/badge/NSF-2132247-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=2132247)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/dcgerard/phwelike/workflows/R-CMD-check/badge.svg)](https://github.com/dcgerard/phwelike/actions)
<!-- badges: end -->

This contains the code from Jiang et al (2021), in package form so it is
easier for me to compare in simulations.

To see the original repo, go to
<https://github.com/LiboJiang/DoubleReduction>

The main functions are

-   `main_multi()`: Fit `main_p2()` across many loci.
-   `main_p2()`: Calculate maximum likelihood esimates of double
    reduction and allele frequency in tetraploids using *all* genotypes.
-   `main_p3()`: Calculate maximum likelihood esimates of double
    reduction and allele frequency in tetraploids using *just*
    heterozygous/homozygous.
-   `sim_p()`: Simulate genotypes at equilibrium in tetraploids.
-   `sim_p3()`: Simulate heterozygous/homozygous at equilibrium in
    tetraploids.

## Acknowledgments

David Gerard’s work was supported by the National Science Foundation
under Grant
No. [2132247](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2132247).
The opinions, findings, and conclusions or recommendations expressed are
those of the author and do not necessarily reflect the views of the
National Science Foundation.

The work of Jiang et al (2021) was supported by grant 201404102 from the
State Administration of Forestry of China and grant 32070601 from the
National Natural Science Foundation of China.

## Code of Conduct

Please note that the phwelike project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

# References

-   Jiang, Libo, Xiangyu Ren, and Rongling Wu. 2021. “Computational
    Characterization of Double Reduction in Autotetraploid Natural
    Populations.” *The Plant Journal* 105 (6): 1703–9.
    [doi:10.1111/tpj.15126](https://doi.org/10.1111/tpj.15126)
