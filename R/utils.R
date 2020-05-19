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
#' Should we use the R system directory for installing \pkg{basilisk}'s conda instance or client environments?
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
#' Should we destroy old installations of conda from previous versions of \pkg{basilisk} or its client packages?
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
#' \code{\link{installConda}}, where this function is used.
#'
#' \code{\link{clearObsoleteDir}}, which may be triggered by this function.
#' 
#' @export
destroyOldVersions <- function() {
    Sys.getenv("BASILISK_NO_DESTROY")!="1"
}

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

.fetch_system_dir <- function(pkgname, installed) {
    if (installed) {
        # This is more robust than .libPaths(), which may change
        # between *basilisk* installation and client installation;
        # system.file() should still pull out the correct dir.
        vdir <- system.file(package=pkgname)
    } else {
        # As this is run in configure, system.file() will not work, as pkgname
        # isn't even installled yet! 
        vdir <- file.path(.libPaths()[1], pkgname)
    }
}

#' Get lock file
#' 
#' Get the path to a lock file, typically used to mark an installation in progress 
#' (or an incomplete installation that needs to be replaced).
#'
#' @param path String containing a path to a conda instance or environment that is to be created.
#'
#' @return String containing a path to a lock file,
#' to be \code{touch}ed before installation starts and deleted after installation finishes \emph{successfully}.
#' 
#' @author Aaron Lun
#' @examples
#' getLockFile("AAAA")
#'
#' @export
getLockFile <- function(path) {
    paste0(sub("/+$", "", path), ".00LOCK")
}

#' Safe file deletion
#'
#' Delete files or directories with an error message if it does not succeed.
#'
#' @param x,recursive,force,... Further arguments to pass to \code{\link{unlink}}.
#'
#' @return
#' Either all \code{x} are successfully deleted or an error is raised.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' This is primarily necessary to avoid incomprehensible errors when 
#' a directory containing a stale environment or installation is not successfully deleted.
#' We set \code{recursive=TRUE} by default for convenience;
#' we also set \code{force=TRUE} by default to avoid difficulties due to rogue permissions.
#'
#' @seealso
#' \code{\link{dir.create2}}, for a similarly safe directory creation function.
#'
#' @examples
#' out <- tempfile()
#' unlink2(out) # no error from deleting non-existent file.
#'
#' write(file=out, "whee")
#' unlink2(out)
#' 
#' @export
unlink2 <- function(x, recursive=TRUE, force=TRUE, ...) {
    status <- unlink(x, recursive=recursive, force=force, ...)
    if (any(failed <- status!=0L)) {
        stop("failed to remove '", x[failed][1], "'")
    }
}

#' Safe directory construction
#' 
#' Create a directory with an error message if it does not succeed.
#' 
#' @param path,recursive,... Further arguments to pass to \code{\link{dir.create}}.
#' 
#' @return Either \code{path} is created or an error is raised.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' This is primarily necessary to avoid incomprehensible errors when 
#' a directory cannot be created, usually due to insufficient permissions.
#' We set \code{recursive=TRUE} by default for convenience.
#'
#' Note that the presence of an existing directory at \code{path}
#' will cause this function to fail.
#' This is usually desirable in the context of \pkg{basilisk.utils}
#' as stale directories should be \code{\link{unlink2}}ed beforehand.
#'
#' @seealso
#' \code{\link{unlink2}}, for a similarly safe deletion function.
#'
#' @examples
#' out <- tempfile()
#' dir.create2(out)
#'
#' @export
dir.create2 <- function(path, recursive=TRUE, ...) {
    if (!dir.create(path, recursive=recursive, ...)) {
        stop("failed to create '", path, "'") 
    }
}
