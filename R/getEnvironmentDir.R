.env_dir <- "basilisk"

#' Get the \pkg{basilisk} environment directory
#'
#' Find the installation directory for the \pkg{basilisk} Python environments.
#'
#' @param pkgname String containing the name of the \pkg{basilisk} client package responsible for generating the environment. 
#' @param assume.installed Logical scalar indicating whether we can assume the environment directoy is already installed.
#'
#' @return String containing the path to the environment directory.
#' If \code{assume.installed=TRUE}, this function will throw an error if the expected directory does not exist.
#'
#' @details
#' By default, \pkg{basilisk} environments are installed to a location specified by \code{\link{getExternalDir}}.
#' This ensures that R package build systems do not attempt to generate binaries that include the Python/conda packages;
#' such binaries are not relocatable due to the presence of hard-coded paths, resulting in run-time failures.
#' 
#' If the \code{BASILISK_USE_SYSTEM_DIR} environment variable is set to \code{"1"},
#' the function will return a path to a location inside the client package's system installation directory.
#' This is the ideal approach when installing from source as we guarantee synchronization in Python and R re-installations.
#' It also ensures that any R process that can load the client package will also have permissions to access its environments,
#' which makes life easier for sysadmins of clusters or other shared resources.
#'
#' @author Aaron Lun
#'
#' @examples
#' # This is the only mode that works in an example,
#' # all other modes rely on installation of the client.
#'
#' # Sys.setenv(BASILISK_USE_SYSTEM_DIR=1)
#' if (useSystemDir()) {
#'    getEnvironmentDir("client.of.basilisk", assume.installed=FALSE)
#' }
#'
#' @export
#' @importFrom utils packageVersion
getEnvironmentDir <- function(pkgname, assume.installed=FALSE) {
    if (!useSystemDir()) {
        vdir <- file.path(getExternalDir(), paste0(pkgname, "-", packageVersion(pkgname)))
    } else {
        if (assume.installed) {
            # This is more robust than .libPaths(), which may change
            # between *basilisk* installation and client installation;
            # system.file() should still pull out the correct dir.
            vdir <- system.file(package=pkgname)
        } else {
            # As this is run in configure, system.file() will not
            # work, as pkgname isn't even installled yet!
            vdir <- file.path(.libPaths()[1], pkgname)
        }
    }
    file.path(vdir, .env_dir)
}
