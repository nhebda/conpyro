
# conpyro

## Conifer Pyrometrics (ConPyro) fire behaviour modelling tools for R

<!-- badges: start -->

![lifecycle](https://img.shields.io/badge/lifecycle-beta-orange)
![license](https://img.shields.io/badge/license-GPL--3-blue)

<!-- badges: end -->

## Overview

`conpyro` is an R package implementing the **Conifer Pyrometrics
(ConPyro)** fire behaviour modelling system described in Perrakis et
al. (2023).

The package provides tools for estimating:

- Fine fuel moisture content using a stand-adjusted model
- Crown fire occurrence probability
- Passive and active crown fire thresholds
- Surface and crown fire rate of spread

The package is designed for both:

- **Single-scenario exploratory modelling**, and
- **Large multi-scenario workflows**, including modeling driven by
  spatial datasets, allowing users to evaluate fire behaviour across
  many fuel and fire weather configurations.

This GitHub release is a **beta version intended for testing and
feedback prior to CRAN submission.**

------------------------------------------------------------------------

## Installation

Install the **beta version** from GitHub:

``` r
install.packages("remotes")
remotes::install_github("nhebda/conpyro")
```

Some optional helper functions rely on the package `cffdrs`. If you plan
to use those tools, install it separately:

``` r
install.packages("cffdrs")
```

Optional plotting requires **ggplot2**.

------------------------------------------------------------------------

## Basic Example

The main modelling function is `conpyro()`, which calculates crown fire
occurrence probability, crowning thresholds, and rate of spread across a
vector of 10-m open wind speeds.

``` r
conpyro(
  data = default_input,
  WS10 = 0:40,
  FFMC = 91,
  DMC = 85
)
```

This calculates fire behaviour metrics for each scenario defined in
`data` across wind speeds from **0–40 km/h**.

------------------------------------------------------------------------

## Input Methods

Inputs can be supplied in two ways:

### 1. Data frame input (recommended for many scenarios)

Each row represents a scenario.

``` r
conpyro(
  data = my_scenarios,
  WS10 = 0:40
)
```

### 2. Direct arguments

Inputs may also be supplied by direct arguments, which can be be length
`1` or the **common length of all scenarios**.

``` r
conpyro(
  WS10 = 0:40,
  FFMC = c(89, 91, 92),
  FSG = 6,
  SFC = 2,
  CBD = 0.1,
  DMC = 80,
  season = c("spring", "summer", "summer"),
  density = "moderate",
  stand = "pine"
)
```

Length `1` arguments will be recycled across all scenarios.

------------------------------------------------------------------------

## Plotting Results

`conpyro()` can optionally generate plots of model outputs.

``` r
conpyro(
  data = default_input,
  WS10 = 0:40,
  FFMC = 91,
  DMC = 85,
  plot = c("pCFO", "ROS")
)
```

Available plot types:

- `"pCFO"` – Crown fire occurrence probability
- `"CAC"` – Criterion for active crowning
- `"ROS"` – Composite rate-of-spread plot with crowning thresholds

Plotting requires **ggplot2**.

------------------------------------------------------------------------

## Helper Tools

The package includes a number of helper functions for calculating model
inputs and intermediate quantities for single scenarios.

### Fine fuel moisture estimation

- `t_mcF()` – FFMC-based fine fuel moisture content
- `t_mcsa()` – Stand-adjusted fine fuel moisture

### Fire modelling

- `t_pCFO()` – Crown fire occurrence probability
- `t_FT()` – Fire type classification
- `t_ROS()` – Equilibrium rate of spread

### Surface fuel consumption

- `t_SFC_FBP()` – FBPS-based SFC
- `t_SFC_deGroot()` – De Groot et al. (2009) SFC equation

### Foliar moisture

- `t_FMC()` – Foliar moisture content estimate

### Experimental ladder fuel tools

- `ladder_standing_dead()`
- `ladder_midstory_saplings()`

These tools allow users to explore individual components of the ConPyro
modeling system.

------------------------------------------------------------------------

## Beta Status

This package is currently in **beta testing**.

During this phase we aim to:

- Collect feedback from fire behaviour researchers and practitioners
- Identify edge cases
- Refine documentation and usability
- Evaluate performance for large-scale modelling workflows

The package interface may evolve prior to a future CRAN release.

------------------------------------------------------------------------

## License

GPL (\>= 3)

------------------------------------------------------------------------

## Contact

Questions, bug reports, and feature requests are welcome.

Please open an issue on GitHub:

    https://github.com/nhebda/conpyro/issues

Or contact:

    Nicholas J. R. Hebda
    Nicholas.Hebda at nrcan-rncan.gc.ca
