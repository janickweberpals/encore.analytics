
<!-- README.md is generated from README.Rmd. Please edit that file -->

# encore.analytics <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/janickweberpals/encore.analytics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janickweberpals/encore.analytics/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/janickweberpals/encore.analytics/workflows/pkgdown/badge.svg)](https://github.com/janickweberpals/encore.analytics/actions/workflows/pkgdown.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/encore.analytics)](https://CRAN.R-project.org/package=encore.analytics)
<!-- badges: end -->

## Overview

In the realm of oncology research, emulating clinical trials using
real-world data presents unique challenges, particularly when dealing
with missing data and the need for careful propensity score analyses.
The `encore.analytics` package addresses these challenges by providing a
comprehensive toolkit that bridges the gap between multiple imputation
and propensity score methodologies.

### Why encore.analytics?

Real-world oncology data often suffers from missing values, requiring
sophisticated imputation techniques. When combined with propensity score
analyses for treatment effect estimation, the complexity increases
substantially. Traditional approaches often handle these challenges
separately, leading to potential biases and methodological
inconsistencies.

The `encore.analytics` package offers an integrated solution that:

- Maintains the proper order of operations between imputation and
  propensity score estimation
- Ensures consistent handling of uncertainty across multiple imputed
  datasets
- Provides specialized tools for oncology-specific outcomes and metrics

The package implements various methodologies for:

- Multiple imputation of missing data
- Propensity score estimation and matching
- Survival analysis with multiply imputed data
- Agreement metrics for assessing consistency across imputed datasets
- Specialized visualization tools for survival outcomes

## Key Features

- **Cox Model Pooling**: Implementation of various approaches for
  pooling Cox proportional hazards models across multiply imputed
  datasets
- **Kaplan-Meier Pooling**: Tools for combining and visualizing
  Kaplan-Meier curves from multiple imputations
- **Raking Weights**: Methods for calibrating weights in propensity
  score analyses
- **Agreement Metrics**: Functions to assess consistency and agreement
  across imputed datasets
- **Table 1 Generation**: Utilities for creating descriptive statistics
  tables
- **Data Simulation**: Tools for generating synthetic datasets for
  testing and validation

## Installation

You can install the development version of encore.analytics from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("janickweberpals/encore.analytics")
```

## Documentation and Examples

For comprehensive documentation and detailed examples, please visit:

- **Package Website**:
  <https://janickweberpals.github.io/encore.analytics/>
- **Detailed Workflows**:
  <https://janickweberpals.github.io/imputation-ps-workflows/>

The documentation includes:

1.  **Background**: Theoretical foundation and methodology
2.  **Multiple Survival Model Comparison**: Approaches for handling
    survival analyses with multiple imputations
3.  **Raking Weights**: Implementation of weight calibration methods
4.  **Kaplan-Meier Pooling**: Techniques for combining survival curves
5.  **Agreement Metrics**: Methods for assessing consistency across
    imputations

## Implementation Details

### Statistical Methodology

The package implements state-of-the-art approaches for:

- Rubin’s rules for combining estimates across imputed datasets
- Various propensity score estimation methods including logistic
  regression and machine learning approaches
- Specialized pooling methods for survival analyses
- Novel metrics for assessing agreement between imputed datasets

### Technical Architecture

Built with extensibility in mind, `encore.analytics` features: - A
modular design that allows for easy addition of new methodologies -
Efficient handling of large datasets through optimized algorithms -
Comprehensive unit testing to ensure reliability - Integration with
popular R packages in the survival analysis ecosystem

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
For major changes, please open an issue first to discuss what you would
like to change.

## Citation

If you use `encore.analytics` in your research, please cite it as:

    Weberpals J (2025). encore.analytics: Multiple Imputation and Propensity Score 
    Workflows for Oncology Trial Emulation. R package version 0.1.0.
    https://github.com/janickweberpals/encore.analytics
