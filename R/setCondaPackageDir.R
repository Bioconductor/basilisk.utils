#' Set or unset the Conda package directory
#'
#' Set or unset the directory used to store the cached Conda packages, e.g., tarballs and such.
#' This should be a non-temporary location as other packages may link to its contents.
#'
#' @param loc A string containing a path to the desired directory (that should already exist).
#' Alternatively \code{NA}, in which case any existing setting is removed.
#'
#' @return The previous value of \code{CONDA_PKGS_DIRS}, invisibly.
#' 
#' @author Aaron Lun
#'
#' @examples
#' # Setting it to something new:
#' out <- setCondaPackageDirectory(tempdir())
#'
#' # Setting it back
#' setCondaPackageDirectory(out)
#' 
#' @export
setCondaPackageDir <- function(loc) {
    if (!is.na(loc)) {
        loc <- normalizePath(loc)
    }
    invisible(setVariable("CONDA_PKGS_DIRS", loc))
}
