#' Install Anaconda 
#'
#' Install Anaconda (version 3, 2019.10) to a destination path that depends on the operating system.
#' This skips the installation if said path already exists.
#'
#' @details
#' This function was originally created from code in \url{https://github.com/hafen/rminiconda},
#' also borrowing code from \pkg{reticulate}'s \code{install_miniconda} for correct Windows installation. 
#'
#' On Unix, Anaconda is installed inside the \pkg{basilisk} installation directory.
#' This is the ideal installation location as it is always cleaned out and replaced when \pkg{basilisk} is reinstalled.
#' It also ensures that any R process that can load \pkg{basilisk} will also have permissions to access the Anaconda instance,
#' which makes life easier for sysadmins of clusters or other shared resources.
#'
#' On MacOSX or Windows, Anaconda is instead installed in the \code{\link{user_data_dir}}.
#' The standard Bioconductor distribution mechanism will provide a binary package for these two operating systems,
#' but the binary will contain hard-coded paths in the Anaconda installation that preclude portable use.
#' Thus, we instead have to install Anaconda on the user machines in a location outside the \pkg{basilisk} installation directory,
#' ensuring that the Bioconductor build system does not attempt to package a binary.
#'
#' Windows has the additional complication that the maximum path length is 260 characters in length.
#' This can result in problems due to Anaconda's long internally nested paths.
#' In such cases, users can redirect Anaconda to install at a shorter path location using the \code{BASILISK_EXTERNAL_DIR} environment variable, which may be sufficient to ensure that the total path length falls under the limit.
#'
#' @section Managing the external installation location:
#' Whenever \code{installAnaconda} is re-run on MacOSX or Windows,
#' the previous Anaconda installation and its various \pkg{basilisk} environments are destroyed.
#' This avoids duplication of Anaconda instances that would otherwise chew up disk space at 3 GB a pop.
#' 
#' After the destruction of the previous instance, we rely on the clients to recreate their environments.
#' They should do this automatically if they are using \pkg{basilisk} correctly.
#'
#' Users can disable this destruction by setting the \code{BASILISK_NO_DESTROY} environment variable to 1.
#' This may be necessary on rare occasions when running multiple R instances on the same Bioconductor release.
#' (Multiple instances using different Bioconductor releases pose no problem.)
#'
#' @return
#' An Anaconda instance is created at an OS-dependent location.
#'  
#' @author Aaron Lun
#'
#' @export
installAnaconda <- function() {
    dest_path <- getBasiliskDir()
    if (file.exists(dest_path)) {
        return(NULL)
    }

    version <- "2019.10"
    base_url <- "https://repo.anaconda.com/archive"

    if (isWindows()) {
        .clear_previous_installs(dest_path)

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
            .clear_previous_installs(dest_path)

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

.clear_previous_installs <- function(dest_path) {
    if (Sys.getenv("BASILISK_NO_DESTROY")!="1") {
        host <- dirname(dest_path)
        major.v <- sub("\\.[0-9]+$", "", basename(host))
        all.candidates <- list.files(dirname(host), full.names=TRUE, pattern=paste0("^", major.v))
        unlink(all.candidates, recursive=TRUE)
    }
}
