#' Get the \pkg{basilisk} Conda directory
#'
#' Find the installation directory for the \pkg{basilisk}-managed Conda instance.
#'
#' @param installed Logical scalar indicating whether \pkg{basilisk} is already installed.
#'
#' @return String containing the path to the conda instance.
#'
#' @details
#' By default, conda is installed to a location specified by \code{\link{getExternalDir}}.
#' This ensures that R package build systems do not attempt to generate binaries that include the conda instance;
#' such binaries are not relocatable due to the presence of hard-coded paths, resulting in run-time failures.
#' 
#' If the \code{BASILISK_EXTERNAL_CONDA} environment variable is set to a path to an existing conda instance,
#' the function will return it directly without modification.
#' This allows users to use their own conda instances with \pkg{basilisk} but, 
#' in turn, they are responsible for managing it.
#'
#' If the \code{BASILISK_USE_SYSTEM_DIR} environment variable is set to \code{"1"},
#' the function will return a path to a location inside the \code{basilisk} system installation directory.
#' This is the ideal approach when installing from source as any conda and \pkg{basilisk} re-installations are synchronized.
#' It also ensures that any R process that can load \pkg{basilisk} will also have permissions to access the conda instance,
#' which makes life easier for sysadmins of clusters or other shared resources.
#'
#' We suggest always calling this function after an \code{\link{installConda}} call,
#' which guarantees the presence of the conda installation directory (or dies trying).
#' Setting \code{installed=FALSE} should only happen inside the \pkg{basilisk} \code{configure} script.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Setting the environment variable to run this example: 
#' # all other modes rely on installation of basilisk.
#' old <- Sys.getenv("BASILISK_USE_SYSTEM_DIR")
#' Sys.setenv(BASILISK_USE_SYSTEM_DIR=1)
#'
#' getCondaDir(installed=FALSE)
#'
#' Sys.setenv(BASILISK_USE_SYSTEM_DIR=old)
#' @export
getCondaDir <- function(installed=TRUE) { 
    inst_path <- .get_external_conda()

    if (is.na(inst_path)) {
        if (!useSystemDir()) {
            inst_path <- getExternalDir()
            inst_path <- file.path(inst_path, "0") # keeping path short for Windows.
        } else {
            inst_path <- getSystemDir("basilisk", installed)
            inst_path <- file.path(inst_path, "conda")
        }
    }

    inst_path
}

.get_external_conda <- function() {
    Sys.getenv("BASILISK_EXTERNAL_CONDA", NA)
}
