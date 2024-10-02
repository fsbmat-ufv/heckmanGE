
<!-- README.md is generated from README.Rmd. Please edit that file -->

# heckmanGE <a href='https://fsbmat-ufv.github.io/heckmanGE/'><img src='man/figures/logo.png' align="right" height="80" style="margin:10px;" /></a>

# heckmanGE

<!-- badges: start -->
<!-- badges: end -->
<p style="text-align: justify;">
The heckmanGE package has functions for modeling data with selection
bias problems. It includes the generalized Heckman model, introduced by
Bastos, Barreto-Souza, and Genton (2022), which allows the inclusion of
covariates to the dispersion and correlation parameters, allowing the
sample selection bias and the dispersion parameters to depend on
covariates. More than that, our package allows the inclusion of sample
weights and the grouping of data into clusters, in such a way that
between clusters the errors are independent, but correlated within each
cluster. The package also allows the adjustment of the classical Heckman
model (Heckman (1976), Heckman (1979)) and the estimation of the
parameters of this model via the maximum likelihood method and the
two-step method.
</p>

## Installation

You can install the released version of heckmanGE from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("heckmanGE")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fsbmat-ufv/heckmanGE")
```

## Code of Conduct

`{heckmanGE}` is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-bastosBarreto" class="csl-entry">

Bastos, Fernando de Souza, Wagner Barreto-Souza, and Marc G Genton.
2022. “A Generalized Heckman Model with Varying Sample Selection Bias
and Dispersion Parameters.” *Statistica Sinica*.

</div>

<div id="ref-heckman1976common" class="csl-entry">

Heckman, James J. 1976. “The Common Structure of Statistical Models of
Truncation, Sample Selection and Limited Dependent Variables and a
Simple Estimator for Such Models.” In *Annals of Economic and Social
Measurement, Volume 5, Number 4*, 475–92. NBER.

</div>

<div id="ref-heckman1979sample" class="csl-entry">

———. 1979. “Sample Selection Bias as a Specification Error.”
*Econometrica: Journal of the Econometric Society*, 153–61.

</div>

</div>
