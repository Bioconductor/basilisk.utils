#' Get external Anaconda installation 
#'
#' Define an external location for installing the Anaconda instance and \pkg{basilisk} environments.
#'
#' @return String containing a path to an appropriate external folder.
#'
#' @details
#' We add the version information so that re-installation of \pkg{basilisk} will install a new instance of Anaconda.
#' (This assumes that \pkg{basilisk} and \pkg{basilisk.utils} have synchronized version bumps.)
#' See \code{\link{installAnaconda}} for more details on how this external location is managed.
#'
#' If the \code{BASILISK_EXTERNAL_DIR} environment variable is set to some location,
#' this will be used instead as the installation directory.
#' Setting this variable is occasionally necessary if the default path returned by \code{\link{user_cache_dir}} has spaces;
#' or on Windows, if the 260 character limit is exceeded after combining the default path with deeply nested Anaconda paths. 
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{getBasiliskDir}}, where this function is used for Windows or MacOSX.
#' 
#' @examples
#' getExternalDir()
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom rappdirs user_cache_dir 
getExternalDir <- function() {
    pkg.v <- as.character(packageVersion("basilisk"))
    inst_path <- Sys.getenv("BASILISK_EXTERNAL_DIR")
    if (inst_path=="") {
        # Using "cache", not "data", as it doesn't have spaces on Mac.
        user_cache_dir(appname="basilisk", appauthor="me", version=pkg.v)
    } else {
        file.path(inst_path, pkg.v)
    }
}
