#' Clear the external installation directory
#'
#' Clear the external installation directory by removing all Anaconda instances 
#' installed for different versions of \pkg{basilisk} with the same middle version number
#' (i.e., same Bioconductor release).
#'
#' @return All Anaconda instances and environments of the same Bioconductor release 
#' as the current \pkg{basilisk} installation are destroyed.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{getExternalDir}}, which determines the location of the external directory.
#'
#' @seealso
#' \code{\link{installAnaconda}}, for the motivation behind this function.
#'
#' @examples
#' clearExternalDir()
#'
#' @export
clearExternalDir <- function() {
    host <- getExternalDir()
    major.v <- sub("\\.[0-9]+$", "", basename(host))
    all.candidates <- list.files(dirname(host), full.names=TRUE, pattern=paste0("^", major.v))
    unlink(all.candidates, recursive=TRUE)
}

