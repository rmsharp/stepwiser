
[![Rdoc](http://www.rdocumentation.org/badges/version/roxygen2)](http://www.rdocumentation.org/packages/roxygen2)
[![Build
Status](https://travis-ci.org/rmsharp/stepwiser.svg?branch=master)](https://travis-ci.org/rmsharp/stepwiser)
[![codecov](https://codecov.io/gh/rmsharp/stepwiser/branch/master/graph/badge.svg)](https://codecov.io/gh/rmsharp/stepwiser)
[![Rdoc](http://www.rdocumentation.org/badges/version/RDocumentation)](http://www.rdocumentation.org/packages/RDocumentation)
[![Rdoc](http://www.rdocumentation.org/badges/version/stepwiser)](http://www.rdocumentation.org/packages/gh/rmsharp/stepwiser)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stepwiser

## Installation

You can install **stepwiser** from github with:

``` r
install.packages("devtools")
devtools::install_github("rmsharp/stepwiser")
```

All missing dependencies should be automatically installed.

Find online documentation at <https://rmsharp.github.io/stepwiser/>.

## Purpose

The use of automated subset search algorithms for the selection of
predictive variables to include in the final model is a standard
practice. This package provides simulation tools (Monte Carlo) and
vignettes using those tools to investigate and expose the weaknesses of
using stepwise methods (backward elimination, forward selection, and
stepwise algorithms).

Despite extensive exposure of the characteristics and limitations of
these stepwise procedures, many users of these algorithms still
attribute more to the methds than they should.

1.  Identify models with minimum residual mean square (MSRES)
2.  Importance is attached to variables as a result of whether or not
    they are included or remain in the model.
3.  Relative importance is associated with the order of entry or
    deletion.

These beliefs are not true and they are not the reason that stepwise
methods were developed. They were developed to select subset from data
sets \`\`padded with extraneous varabiles’’. This package can be used to
examine how successful stepwise methods are in attaining that goal.
