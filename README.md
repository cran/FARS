# FARS - Factor-Augmented Regression Scenarios

<!-- badges: start -->
[![R-CMD-check](https://github.com/GPEBellocca/FARS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GPEBellocca/FARS/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/FARS)](https://CRAN.R-project.org/package=FARS)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/FARS)
<!-- badges: end -->

The `FARS` package provides a comprehensive framework in R for modeling and forecasting economic scenarios based on the multi-level dynamic factor model (MLDFM). The package enables users to:

- (i) Extract global and group-specific factors using a flexible multi-level factor structure.
- (ii) Compute asymptotically valid confidence regions for the estimated factors, accounting for uncertainty in the factor loading.
- (iii) Obtain estimates of the parameters of the factor-augmented quantile regressions together with their standard deviations.
- (iv) Recover full predictive conditional densities from estimated quantiles.
- (v) Obtain risk measures based on extreme quantiles of the conditional densities.
- (vi) estimate the conditional density and the corresponding extreme quantiles when the factors are stressed.

# Installation and Usage
For detailed usage and examples please refer to the [FARS Vignette](https://arxiv.org/abs/2507.10679).
The Vignette llustrates the functionalities of the FARS package by extracting factors, estimating conditional densities, and constructing stressed scenarios in two applications:

- (i) Aggregate inflation in Europe
- (ii) Building growth density scenarios for the United States (replicating González-Rivera, G., Rodríguez-Caballero, C. V., & Ruiz, E., 2024. Expecting the unexpected: Stressed scenarios for economic growth. Journal of Applied Econometrics, 39(5), 926–942. https://doi.org/10.1002/jae.3060)

![FARS Logo](https://gpebellocca.weebly.com/uploads/1/4/3/4/143433954/fars-logo-copia_orig.png)
