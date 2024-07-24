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
#' Setting this variable is occasionally necessary if the default path returned by \code{\link{R_user_dir}} has spaces;
#' or on Windows, if the 260 character limit is exceeded after combining the default path with deeply nested conda paths. 
#'
#' We assume that the user has read-write access to the external directory.
#' Write access is necessary to generate new environments and to handle locking in \code{\link{lockExternalDir}}.
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
#' @importFrom tools R_user_dir
getExternalDir <- function() {
    inst_path <- Sys.getenv("BASILISK_EXTERNAL_DIR", NA)
    if (is.na(inst_path)) {
        if (isWindows()) {
            # As of Miniforge3 24.3.0-0, the Windows installer doesn't allow
            # paths longer than 46 characters, so just throw it in the user's
            # home directory and hope for the best.
            inst_path <- path.expand("~")
            if (basename(inst_path) == "Documents") {
                inst_path <- dirname(inst_path)
            }
            inst_path <- file.path(inst_path, ".basilisk")

            # Oh, and the Windows installer needs paths with a backslash.
            inst_path <- gsub("/", "\\", inst_path)

        } else {
            inst_path <- R_user_dir("basilisk", "cache")
        }
    }
    pkg.v <- as.character(packageVersion("basilisk"))
    file.path(path.expand(inst_path), pkg.v)
}
