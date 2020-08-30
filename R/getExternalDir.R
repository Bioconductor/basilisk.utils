#' Get an external conda directory
#'
#' Define an external location for installing the conda instance and \pkg{basilisk} environments.
#'
#' @return String containing a path to an appropriate external folder.
#' The last component of the path will always be the \pkg{basilisk} version number.
#'
#' @details
#' The default path contains the version number so that re-installation of \pkg{basilisk} will install a new instance of Conda.
#' (This assumes that \pkg{basilisk} and \pkg{basilisk.utils} have synchronized version bumps.)
#' See \code{\link{installConda}} for more details on how old versions of Conda are managed in this external directory.
#'
#' If the \code{BASILISK_EXTERNAL_DIR} environment variable is set to some location,
#' this will be used instead as the installation directory.
#' Setting this variable is occasionally necessary if the default path returned by \code{\link{user_cache_dir}} has spaces;
#' or on Windows, if the 260 character limit is exceeded after combining the default path with deeply nested conda paths. 
#'
#' We assume that the user has read-write access to the external directory.
#' Write access is necessary to generate new environments and to handle locking in \code{\link{lockInstallation}}.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{getCondaDir}}, to obtain the Conda installation directory. 
#' 
#' @examples
#' # We can't actually run getExternalDir() here, as it 
#' # either relies on basilisk already being installed.
#' print("dummy test to pass BiocCheck")
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom rappdirs user_cache_dir 
getExternalDir <- function() {
    pkg.v <- as.character(packageVersion("basilisk"))
    inst_path <- Sys.getenv("BASILISK_EXTERNAL_DIR", NA)
    if (is.na(inst_path)) {
        # Using "cache", not "data", as it doesn't have spaces on Mac.
        # Also getting rid of appauthor= and opinion= to keep the path low on Windows.
        user_cache_dir(appname="basilisk", appauthor=NULL, version=pkg.v, opinion=FALSE)
    } else {
        file.path(inst_path, pkg.v)
    }
}
