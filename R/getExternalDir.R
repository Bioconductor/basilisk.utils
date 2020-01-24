#' Get external Anaconda installation 
#'
#' Define an external location for installing the Anaconda instance, 
#' usually on Windows or MacOSX. 
#'
#' @return String containing a path to an appropriate external folder.
#'
#' @details
#' We add the version information so that re-installation of \pkg{basilisk} will install a new instance of Anaconda.
#' (This assumes that \pkg{basilisk} and \pkg{basilisk.utils} have synchronized version bumps.)
#' See \code{\link{installAnaconda}} for more details on how this external location is managed.
#' 
#' This function will respect any setting of \code{BASILISK_EXTERNAL_DIR}, 
#' see \code{?\link{installAnaconda}} for more details.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{basiliskAnaconda}}, where this function is used for Windows or MacOSX.
#' 
#' @examples
#' getExternalDir()
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom rappdirs user_data_dir
getExternalDir <- function() {
    pkg.v <- as.character(packageVersion("basilisk.utils"))
    inst_path <- Sys.getenv("BASILISK_EXTERNAL_DIR")
    if (inst_path=="") {
        user_data_dir(appname="basilisk", appauthor="me", version=pkg.v)
    } else {
        file.path(inst_path, pkg.v)
    }
}
