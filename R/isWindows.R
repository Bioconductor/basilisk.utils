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
