#' Find the operating system
#'
#' Indicate whether we are on Windows or MacOSX.
#'
#' @return Logical scalar indicating whether we are on the specified OS.
#'
#' @author Aaron Lun
#'
#' @examples
#' isWindows()
#' isMacOSX()
#' @export
isWindows <- function() {
    .Platform$OS.type=="windows" 
}

#' @export
#' @rdname isWindows
isMacOSX <- function() {
    Sys.info()[["sysname"]] == "Darwin"
}

#' Use the R system directory?
#'
#' Should we use the R system directory for installing \pkg{basilisk}'s Anaconda instance or client environments?
#'
#' @return Logical scalar providing an answer to the above.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{getBasiliskDir}} and \code{\link{getEnvironmentDir}}, where these functions are used.
#'
#' @export
useSystemDir <- function() {
    identical(Sys.getenv("BASILISK_USE_SYSTEM_DIR"), "1")
}
