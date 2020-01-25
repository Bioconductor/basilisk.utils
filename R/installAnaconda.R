#' Install Anaconda 
#'
#' Install Anaconda (version 3, 2019.10) to a destination path that depends on the operating system.
#' This skips the installation if said path already exists.
#'
#' @details
#' This function was originally created from code in \url{https://github.com/hafen/rminiconda},
#' also borrowing code from \pkg{reticulate}'s \code{install_miniconda} for correct Windows installation.
#' We use \pkg{BiocFileCache} if available to avoid redownloading the Anaconda installer upon \pkg{basilisk} re-installation.
#'
#' Whenever \code{installAnaconda} is re-run (and \code{BASILISK_USE_SYSTEM_DIR} is not set, see \code{?\link{getBasiliskDir}}),
#' the previous Anaconda installation and its various \pkg{basilisk} environments are destroyed.
#' This avoids duplication of Anaconda instances that would otherwise chew up disk space at 3 GB a pop.
#' 
#' After the destruction of the previous instance, we rely on the client packages to recreate their required environments.
#' They should do this automatically if they are using \pkg{basilisk} correctly.
#'
#' Users can disable this destruction by setting the \code{BASILISK_NO_DESTROY} environment variable to \code{"1"}.
#' This may be necessary on rare occasions when running multiple R instances on the same Bioconductor release.
#' (Setting this variable is not required for instances using different Bioconductor releases.)
#'
#' @return
#' An Anaconda instance is created at the location specified by \code{\link{getBasiliskDir}}.
#'  
#' @author Aaron Lun
#'
#' @examples
#' # We can't actually run installAnaconda() here, as it 
#' # hard-codes installation into basilisk's directory on
#' # Linux (which is desirable in the wider context).
#' print("dummy test to pass BiocCheck")
#'
#' @export
installAnaconda <- function() {
    dest_path <- getBasiliskDir(assume.installed=FALSE)
    if (file.exists(dest_path)) {
        return(NULL)
    }

    if (!useSystemDir() && Sys.getenv("BASILISK_NO_DESTROY")!="1") {
        clearExternalDir()
    }

    version <- "2019.10"
    base_url <- "https://repo.anaconda.com/archive"


    if (isWindows()) {
        arch <- if (.Machine$sizeof.pointer == 8) "x86_64" else "x86"
        inst_file <- sprintf("Anaconda3-%s-Windows-%s.exe", version, arch)
        tmploc <- .expedient_download(file.path(base_url, inst_file))

        # Using the same code as reticulate:::miniconda_installer_run.
        dir.create(dest_path, recursive = TRUE, showWarnings = FALSE)
        inst_args <- sprintf("/InstallationType=JustMe /RegisterPython=0 /S /D=%s", utils::shortPathName(dest_path))
        Sys.chmod(tmploc, mode = "0755")
        status <- system2(tmploc, inst_args)

    } else {
        is_mac <- isMacOSX()
        sysname <- if (is_mac) "MacOSX" else "Linux"
        inst_file <- sprintf("Anaconda3-%s-%s-x86_64.sh", version, sysname)

        tmploc <- .expedient_download(file.path(base_url, inst_file))
        inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path)

        if (is_mac) {
            # The prebuilt R binary for Mac seems to set this variable,
            # which causes default paths for zlib to be ignored and breaks
            # installation. So, we unset it before attempting installation.
            status <- system(paste("unset DYLD_FALLBACK_LIBRARY_PATH; bash", inst_args))
        } else {
            status <- system2("bash", inst_args)
        }
    }

    if (status != 0) {
        stop(sprintf("conda installation failed with status code '%s'", status))
    }

    NULL
}

#' @importFrom utils download.file
#' @importFrom methods is
.expedient_download <- function(url) {
    fname <- try({
        bfc <- BiocFileCache::BiocFileCache(ask=FALSE)
        BiocFileCache::bfcrpath(bfc, url) 
    })

    if (is(fname, "try-error")) {
        tmploc <- file.path(tempdir(), basename(url))
        download.file(url, tmploc)
        fname <- tmploc
    }

    fname
}
