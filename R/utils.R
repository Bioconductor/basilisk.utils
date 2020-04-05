#' Find the operating system
#'
#' Indicate whether we are on Windows or MacOSX.
#'
#' @return Logical scalar indicating whether we are on the specified OS.
#'
#' @author Aaron Lun
#'
#' @examples
#' isWindows()
#' isMacOSX()
#' @export
isWindows <- function() {
    .Platform$OS.type=="windows" 
}

#' @export
#' @rdname isWindows
isMacOSX <- function() {
    Sys.info()[["sysname"]] == "Darwin"
}

#' Use the R system directory?
#'
#' Should we use the R system directory for installing \pkg{basilisk}'s Anaconda instance or client environments?
#'
#' @details
#' The default value is \code{FALSE} to avoid problems with position-dependent code in packaged binaries.
#' This can be changed by setting \code{BASILISK_USE_SYSTEM_DIR} environment variable to \code{"1"}.
#' 
#' @return Logical scalar providing an answer to the above.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{getBasiliskDir}} and \code{\link{getEnvironmentDir}}, where this function is used.
#'
#' @export
useSystemDir <- function() {
    identical(Sys.getenv("BASILISK_USE_SYSTEM_DIR"), "1")
}

#' Destroy old versions?
#' 
#' Should we destroy old installations of Anaconda from previous versions of \pkg{basilisk} or its client packages?
#'
#' @details
#' The default value is \code{TRUE}, in order to save some hard drive space.
#' This can be changed by setting \code{BASILISK_NO_DESTROY} environment variable to \code{"1"}.
#' 
#' @return Logical scalar providing an answer to the above.
#'
#' @author Aaron Lun
#'
#' @seealso 
#' \code{\link{installAnaconda}}, where this function is used.
#'
#' \code{\link{clearObsoleteDir}}, which may be triggered by this function.
#' 
#' @export
destroyOldVersions <- function() {
    Sys.getenv("BASILISK_NO_DESTROY")!="1"
}

#' Get binary paths
#'
#' @param loc String containing the path to the root of a conda installation or environment.
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

.lock_file <- function(path) {
    paste0(sub("/+$", "", path), ".00basilock")
}
