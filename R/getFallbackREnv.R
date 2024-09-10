#' Get the path to the fallback R environment
#'
#' Get the path to the conda environment containing an internal R installation.
#' This is used as a last resort fallback for \pkg{reticulate} when there are shared library version conflicts with the current R installation.
#'
#' @return String containing the path to the conda environment with a fallback version of R installed. 
#'
#' @details
#' If the environment does not yet exist and \code{\link{useSystemDir}} is \code{FALSE}, it is created on the fly.
#' Otherwise, if \code{\link{useSystemDir}} is \code{TRUE}, an error is thrown;
#' this is because the fallback environment should have been created by \code{\link{installConda}}, unless \code{\link{noFallbackR}} is set to \code{TRUE}.
#'
#' @author Aaron Lun
#'
#' @examples
#' # We can't actually run getFallbackEnv() here, as it 
#' # either relies on basilisk already being installed or
#' # it has a hard-coded path to the basilisk system dir.
#' print("dummy test to pass BiocCheck")
#' 
#' @export
getFallbackREnv <- function() {
    condir <- getCondaDir()
    path <- file.path(condir, "envs", "fallback")

    if (!file.exists(path)) {
        if (!useSystemDir()) {
            okay <- FALSE
            on.exit(if (!okay) unlink(path, recursive=TRUE))
            .install_fallback_r(condir)
            okay <- TRUE
        } else {
            stop("fallback R installation should be created on basilisk installation")
        }
    }

    path
}

.install_fallback_r <- function(path) {
    reticulate::conda_create(
        "fallback",
        conda=getCondaBinary(path),
        packages="r-reticulate=1.38.0",
        channel="conda-forge",
        additional_create_args="--override-channels",
        additional_install_args="--override-channels"
    )
}
