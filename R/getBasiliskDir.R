.core_dir <- "anaconda"

#' Get the \pkg{basilisk} Anaconda directory
#'
#' Find the installation directory for the \pkg{basilisk}-managed Anaconda instance.
#'
#' @param mustWork Logical scalar indicating whether to throw an error if the directory does not exist.
#'
#' @return String containing the path to the Anaconda instance.
#'
#' @details
#' If the \code{BASILISK_USE_ANACONDA} environment variable is set,
#' the function will return it directly without modification.
#' This allows users to use their own Anaconda instances with \pkg{basilisk} but, 
#' in turn, they are responsible for managing it.
#'
#' @author Aaron Lun
#'
#' @examples
#' getBasiliskDir(mustWork=FALSE)
#'
#' @export
getBasiliskDir <- function(mustWork=TRUE) {
    inst_path <- Sys.getenv("BASILISK_USE_ANACONDA")
    if (identical(inst_path, "")) {
        if (isWindows() || isMacOSX()) {
            inst_path <- getExternalDir()
        } else {
            # Can't use system.file(), basilisk isn't installed yet!
            inst_path <- file.path(.libPaths()[1], "basilisk")
        }
        inst_path <- file.path(inst_path, .core_dir)
    }

    if (mustWork && !file.exists(inst_path)) {
        stop("basilisk installation directory does not exist")
    }

    inst_path
}
