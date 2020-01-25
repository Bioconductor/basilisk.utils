.core_dir <- "anaconda"

#' Get the \pkg{basilisk} Anaconda directory
#'
#' Find the installation directory for the \pkg{basilisk}-managed Anaconda instance.
#'
#' @param assume.installed Logical scalar indicating whether we can assume the Anaconda instance is already installed.
#'
#' @return String containing the path to the Anaconda instance.
#' If \code{assume.installed=TRUE}, this function will throw an error if the expected directory does not exist.
#'
#' @details
#' By default, Anaconda is installed to a location specified by \code{\link{getExternalDir}}.
#' This ensures that R package build systems do not attempt to generate binaries that include the Anaconda installation;
#' such binaries are not relocatable due to the presence of hard-coded paths, resulting in run-time failures.
#' 
#' If the \code{BASILISK_EXTERNAL_ANACONDA} environment variable is set to a path to an existing Anaconda installation,
#' the function will return it directly without modification.
#' This allows users to use their own Anaconda instances with \pkg{basilisk} but, 
#' in turn, they are responsible for managing it.
#'
#' If the \code{BASILISK_USE_SYSTEM_DIR} environment variable is set to \code{"1"},
#' the function will return a path to a location inside the \code{basilisk} system installation directory.
#' This is the ideal approach when installing from source as any Anaconda and \pkg{basilisk} re-installations are synchronized.
#' It also ensures that any R process that can load \pkg{basilisk} will also have permissions to access the Anaconda instance,
#' which makes life easier for sysadmins of clusters or other shared resources.
#'
#' @author Aaron Lun
#'
#' @examples
#' # This is the only mode that works in an example,
#' # all other modes rely on installation of basilisk.
#'
#' # Sys.setenv(BASILISK_USE_SYSTEM_DIR=1)
#' if (useSystemDir()) {
#'     getBasiliskDir(assume.installed=FALSE)
#' }
#'
#' @export
getBasiliskDir <- function(assume.installed=TRUE) {
    inst_path <- Sys.getenv("BASILISK_EXTERNAL_ANACONDA")
    if (identical(inst_path, "")) {
        if (!useSystemDir()) {
            inst_path <- getExternalDir()
        } else {
            if (assume.installed) {
                # Uses the correct location if .libPaths() has been changed.
                inst_path <- system.file(package="basilisk")
            } else {
                # Can't use system.file(), basilisk isn't installed yet!
                inst_path <- file.path(.libPaths()[1], "basilisk")
            }
        }
        inst_path <- file.path(inst_path, .core_dir)
    }

    if (assume.installed && !file.exists(inst_path)) {
        stop("basilisk installation directory does not exist")
    }

    inst_path
}
