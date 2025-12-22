# Install package on demand

This function checks if a package is installed. If not, it asks the user
if they want to install it. If the user agrees, it installs the package
from CRAN.

## Usage

``` r
install_on_demand(pkg, quiet = FALSE, ...)
```

## Arguments

- pkg:

  a character string with the name of the package

- quiet:

  logical. If TRUE, suppresses messages during

- ...:

  additional arguments passed to `install.packages`
