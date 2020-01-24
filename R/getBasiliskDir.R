.core_dir <- "anaconda"

#' Get the \pkg{basilisk} Anaconda directory
#'
#' Find the installation directory for the \pkg{basilisk}-managed Anaconda instance.
#'
#' @param assume.installed Logical scalar indicating whether we can assume the Anaconda instance is already installed.
#'
#' @return String containing the path to the Anaconda instance.
#'
#' @details
#' If the \code{BASILISK_USE_ANACONDA} environment variable is set,
#' the function will return it directly without modification.
#' This allows users to use their own Anaconda instances with \pkg{basilisk} but, 
#' in turn, they are responsible for managing it.
#'
#' If \code{assume.installed=TRUE}, this function will throw an error if the Anaconda directory does not exist.
#' On Unix, it will also use \code{\link{system.file}} to ensure that the correct directory is recovered,
#' even if \code{\link{.libPaths}} is altered to some other installation location.
#' 
#' @author Aaron Lun
#'
#' @examples
#' getBasiliskDir(assume.installed=FALSE)
#'
#' @export
getBasiliskDir <- function(assume.installed=TRUE) {
    inst_path <- Sys.getenv("BASILISK_USE_ANACONDA")
    if (identical(inst_path, "")) {
        if (isWindows() || isMacOSX()) {
            inst_path <- getExternalDir()
        } else {
            if (assume.installed) {
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
