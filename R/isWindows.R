#' Find the operating system or architecture.
#'
#' Indicate whether we are on Windows or MacOSX.
#' For MacOSX, we can also determine if we are on an x86-64 or Arm-based architecture.
#'
#' @return Logical scalar indicating whether we are on the specified OS and/or architecture.
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

#' @export
#' @rdname isWindows
isMacOSXArm <- function() {
    grepl("^arm", Sys.info()[["machine"]])
}
