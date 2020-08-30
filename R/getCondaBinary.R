#' Get binary paths
#'
#' @param loc String containing the path to the root of a conda instance or environment.
#'
#' @return String containing the path to the conda or Python executable inside \code{loc}.
#' If \code{loc} is not supplied, the relative path from the root of the environment is returned.
#'
#' @details
#' This code is largely copied from \pkg{reticulate},
#' and is only present here as they do not export these utilities for general consumption.
#'
#' @author Aaron Lun
#'
#' @examples
#' getCondaBinary()
#'
#' getPythonBinary()
#' @name getBinaries
#' @export
getCondaBinary <- function(loc) {
    suffix <- if (isWindows()) "Scripts/conda.exe" else "bin/conda"

    if (missing(loc)) suffix else file.path(loc, suffix)
}

#' @export
#' @rdname getBinaries
getPythonBinary <- function(loc) {
    suffix <- if (isWindows()) "python.exe" else "bin/python"

    if (missing(loc)) suffix else file.path(loc, suffix)
}
