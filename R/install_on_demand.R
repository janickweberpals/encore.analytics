#' Install package on demand
#'
#' @description
#' This function checks if a package is installed. If not, it
#' asks the user if they want to install it. If the user
#' agrees, it installs the package from CRAN.
#'
#' @param pkg a character string with the name of the package
#' @param quiet logical. If TRUE, suppresses messages during
#' @param ... additional arguments passed to \code{install.packages}
#'
install_on_demand <- function(pkg, quiet = FALSE, ...) {
  # internal function that checks whether package pkg is
  # in the library. If not found, it asks the user permission
  # to install.
  if (requireNamespace(pkg, quietly = TRUE)) {
    return()
  }
  if (interactive()) {
    answer <- utils::askYesNo(paste("Package", pkg, "needed. Install from CRAN?"))
    if (answer) utils::install.packages(pkg, quiet = quiet)
  }
}
