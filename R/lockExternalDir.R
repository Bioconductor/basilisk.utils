#' Lock external directory 
#'
#' Lock the external Conda installation directory so that multiple processes cannot try to install at the same time.
#'
#' @param ... Further arguments to pass to \code{\link{lock}}, such as \code{exclusive}.
#' @param lock An existing \code{filelock_lock} object.
#'
#' @return 
#' \code{lockExternalDir} will return a \code{filelock_lock} object from \code{\link{lock}}.
#' 
#' \code{unlockExternalDir} will unlock the file and return \code{NULL} invisibly.
#'
#' @details
#' This will apply a lock to the (possibly user-specified) external directory,
#' so that a user trying to run parallel \pkg{basilisk} processes will not have race conditions during lazy Conda installation.
#' We suggest applying the lock at the start of the function, 
#' ensuring that we wait for the completion of any installation process that might be operating on the directory.
#'
#' Note that locking is only required during installation of Conda (or its environments), not during their actual use.
#' Once an installation/environment is created, we assume that it is read-only for all processes.
#' Technically, this might not be true if one were to install a new version of \pkg{basilisk} halfway through an R session,
#' which would prompt \code{\link{installConda}} to wipe out the old Conda installations;
#' but one cannot in general guarantee the behavior of running R sessions when package versions change anyway,
#' so we won't bother to protect against that.
#'
#' If a system installation is being used, we do not need to lock as the R package manager should avoid race conditions.
#'
#' @author Aaron Lun
#'
#' @examples
#' loc <- lockExternalDir()
#' unlockExternalDir(loc)
#'
#' @export
#' @importFrom filelock lock 
lockExternalDir <- function(...) {
    # Global lock, going above the version number in getExternalDir().
    # This is because getExternalDir() itself might get deleted in
    # installConda(), and you can't lock a file in a non-existent dir.
    dir <- dirname(getExternalDir()) 
    dir.create(dir, recursive=TRUE, showWarnings=FALSE)
    lock.path <- file.path(dir, "00LOCK")
    lock(lock.path, ...)
}

#' @export
#' @rdname lockExternalDir
#' @importFrom filelock unlock
unlockExternalDir <- function(lock) {
    if (!is.null(lock)) {
        unlock(lock)
    }
    invisible(NULL)
}
