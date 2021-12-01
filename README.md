
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dispositionEffect <a href='https://marcozanotti.github.io/dispositionEffect/index.html'><img src="man/figures/logo.png" align="right" height="200"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dispositionEffect)](https://CRAN.R-project.org/package=dispositionEffect)
![](https://cranlogs.r-pkg.org/badges/dispositionEffect?color=brightgreen)
![](http://cranlogs.r-pkg.org/badges/grand-total/dispositionEffect?color=brightgreen)
[![R build
status](https://github.com/marcozanotti/dispositionEffect/workflows/R-CMD-check/badge.svg)](https://github.com/marcozanotti/dispositionEffect/actions)
[![Codecov test
coverage](https://codecov.io/gh/marcozanotti/dispositionEffect/branch/main/graph/badge.svg)](https://codecov.io/gh/marcozanotti/dispositionEffect?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Website](https://img.shields.io/website?down_color=red&down_message=offline&up_color=green&up_message=online&url=https%3A%2F%2Fmarcozanotti.github.io%2FdispositionEffect%2F)](https://marcozanotti.github.io/dispositionEffect/index.html)
[![GitHub
issues](https://img.shields.io/github/issues/marcozanotti/dispositionEffect)](https://github.com/marcozanotti/dispositionEffect/issues)
![GitHub R package
version](https://img.shields.io/github/r-package/v/marcozanotti/dispositionEffect)
![GitHub top
language](https://img.shields.io/github/languages/top/marcozanotti/dispositionEffect)
<!-- badges: end -->

The `dispositionEffect` package allows to quickly evaluate the presence
of disposition effect’s behaviors of an investor based solely on his
transactions and the market prices of the traded assets.

## Installation

You can install the released version of `dispositionEffect` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dispositionEffect")
```

Otherwise, you can also install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("marcozanotti/dispositionEffect")
```

## Overview

The package contains few user-friendly purpose specific interfaces:

-   `portfolio_compute` is a wrapper function that compute realized and
    paper gains and losses from the investor’s transactions and the
    market prices of the traded assets and updates the investor’s
    portfolio

-   `gains_losses` is the core function of the package. It performs all
    the necessary calculations and can be used for real-time processing
    (it is intended for advanced users only)

-   `disposition_effect` computes the disposition effect

-   `disposition_difference` computes the disposition difference

-   `disposition_compute`and `disposition_summary`interfaces that allow
    to easily compute disposition effect and summary statistics.

## Tutorials

-   [Getting
    started](https://marcozanotti.github.io/dispositionEffect/articles/getting-started.html)

-   [The Analysis of Disposition
    Effect](https://marcozanotti.github.io/dispositionEffect/articles/de-analysis.html)

-   [Disposition Effect in
    Parallel](https://marcozanotti.github.io/dispositionEffect/articles/de-parallel.html)

-   [Time Series Disposition
    Effect](https://marcozanotti.github.io/dispositionEffect/articles/de-timeseries.html)

## References

-   Mazzucchelli and Zanotti, 2021, *Mean Reverting Expectations can
    help in Rationalizing the Disposition Effect* (working paper)

-   Mazzucchelli and Zanotti, 2021, *The Portfolio Driven Disposition
    Effect and the Short Selling* (working paper)

-   Mazzucchelli, 2021, *An Analysis of Short Selling and Volatility
    Impact on the Disposition Effect* (working paper)

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/marcozanotti/dispositionEffect/issues).

For questions and other discussion, mail us at
<zanottimarco17@gmail.com>.

## Acknowledgements

A special thank to [Claud
Graphics](https://www.behance.net/claudiocec3c4f) for our logo.
