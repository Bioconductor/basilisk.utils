#' Safe file deletion
#'
#' Delete files or directories with an error message if it does not succeed.
#'
#' @param x,recursive,force,... Further arguments to pass to \code{\link{unlink}}.
#'
#' @return
#' Either all \code{x} are successfully deleted or an error is raised.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' This is primarily necessary to avoid incomprehensible errors when 
#' a directory containing a stale environment or installation is not successfully deleted.
#' We set \code{recursive=TRUE} by default for convenience;
#' we also set \code{force=TRUE} by default to avoid difficulties due to rogue permissions.
#'
#' @seealso
#' \code{\link{dir.create2}}, for a similarly safe directory creation function.
#'
#' @examples
#' out <- tempfile()
#' unlink2(out) # no error from deleting non-existent file.
#'
#' write(file=out, "whee")
#' unlink2(out)
#' 
#' @export
unlink2 <- function(x, recursive=TRUE, force=TRUE, ...) {
    status <- unlink(x, recursive=recursive, force=force, ...)
    if (any(failed <- status!=0L)) {
        stop("failed to remove '", x[failed][1], "'")
    }
}

#' Safe directory construction
#' 
#' Create a directory with an error message if it does not succeed.
#' 
#' @param path,recursive,... Further arguments to pass to \code{\link{dir.create}}.
#' 
#' @return Either \code{path} is created or an error is raised.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' This is primarily necessary to avoid incomprehensible errors when 
#' a directory cannot be created, usually due to insufficient permissions.
#' We set \code{recursive=TRUE} by default for convenience.
#'
#' Note that the presence of an existing directory at \code{path}
#' will cause this function to fail.
#' This is usually desirable in the context of \pkg{basilisk.utils}
#' as stale directories should be \code{\link{unlink2}}ed beforehand.
#'
#' @seealso
#' \code{\link{unlink2}}, for a similarly safe deletion function.
#'
#' @examples
#' out <- tempfile()
#' dir.create2(out)
#'
#' @export
dir.create2 <- function(path, recursive=TRUE, ...) {
    if (!dir.create(path, recursive=recursive, ...)) {
        stop("failed to create '", path, "'") 
    }
}
