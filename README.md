
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://opensource.org/licenses/MIT)
[![Release](https://img.shields.io/github/release/b-cubed-eu/simcuber.svg)](https://github.com/b-cubed-eu/simcuber/releases)
![GitHub](https://img.shields.io/github/license/b-cubed-eu/simcuber)
[![R build
status](https://github.com/b-cubed-eu/simcuber/workflows/check%20package%20on%20main/badge.svg)](https://github.com/b-cubed-eu/simcuber/actions)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/b-cubed-eu/simcuber.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/b-cubed-eu/simcuber.svg)
[![R-CMD-check](https://github.com/b-cubed-eu/simcuber/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/b-cubed-eu/simcuber/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/b-cubed-eu/simcuber/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/b-cubed-eu/simcuber/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

# simcuber

The goal of **simcuber** is to provide a simulation framework for
biodiversity data cubes using the R programming language. This can start
from simulating multiple species distributed in a landscape over a
temporal scope. In a second phase, the simulation of a variety of
observation processes and effort can generate actual occurrence
datasets. Based on their (simulated) spatial uncertainty, occurrences
can then be designated to a grid to form a data cube.

Simulation studies offer numerous benefits due to their ability to mimic
real-world scenarios in controlled and customizable environments.
Ecosystems and biodiversity data are very complex and involve a
multitude of interacting factors. Simulations allow researchers to model
and understand the complexity of ecological systems by varying
parameters such as spatial and/or temporal clustering, species
prevalence, etc.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("b-cubed-eu/simcuber")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(simcuber)
```

…
