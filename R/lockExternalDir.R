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
#' This will apply a lock to the (possibly user-specified) external Conda installation directory,
#' so that a user trying to run parallel \pkg{basilisk} processes will not have race conditions during lazy Conda installation.
#' We use \pkg{filelock} to manage the locking process for us, with the following strategy:
#' \itemize{
#' \item If a system installation is being performed, we do not perform any locking.
#' Rather, the R package manager will lock the entire R installation directory for us.
#' \item If the external directory is not yet present, we establish an exclusive lock.
#' We then proceed to the creation of said directory and installation of Conda.
#' \item If an external installation directory is already present, we establish a shared lock.
#' This will wait for any exclusive lock to expire (and thus any currently running installation to finish).
#' No waiting is required if there are no existing exclusive locks.
#' }
#'
#' Note that locking is only required during installation of Conda (or its environments), not during actual use.
#' Once an installation/environment is created, we assume that it is read-only for all processes.
#' Technically, this might not be true if one were to install a new version of \pkg{basilisk} halfway through an R session,
#' which would prompt \code{\link{installConda}} to wipe out the old Conda installations;
#' but one cannot in general guarantee the behavior of running R sessions when package versions change anyway,
#' so we won't bother to protect against that.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{installConda}}, for an example of how to implement this locking approach.
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
