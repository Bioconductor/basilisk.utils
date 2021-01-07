#' Run \code{conda clean}
#'
#' Clean the Conda installation to remove unused packages and tarballs.
#'
#' @param loc String containing the path to the root of a conda instance or environment.
#'
#' @return An integer value indicating whether the cleaning was successful.
#'
#' @details
#' This should only be performed to save disk space for system installations, 
#' as the cached extras will never be used by any other \pkg{basilisk} package.
#'
#' In contrast, for an externally cached conda, the extras may be re-used by other \pkg{basilisk} clients.
#' So it's usually worth keeping them around, though this function can be called directly by the user if the cache gets too big.
#'
#' @author Aaron Lun
#' 
#' @export 
cleanConda <- function(loc=getCondaDir()) {
    listing <- activateEnvironment()
    on.exit(deactivateEnvironment(listing))
    system2(getCondaBinary(loc), c("clean", "-ayq"))
}
