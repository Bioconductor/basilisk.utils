#' Install Miniconda 
#'
#' Install Miniconda (version 3, 2019.10) to a destination path that depends on the operating system.
#' This skips the installation if said path already exists.
#'
#' @param installed Logical scalar indicating whether \pkg{basilisk} is already installed.
#' Should only be set to \code{FALSE} in \pkg{basilisk} \code{configure} scripts.
#' 
#' @details
#' This function was originally created from code in \url{https://github.com/hafen/rminiconda},
#' also borrowing code from \pkg{reticulate}'s \code{install_miniconda} for correct Windows installation.
#' We use \pkg{BiocFileCache} if available to avoid redownloading the Miniconda installer upon \pkg{basilisk} re-installation.
#'
#' Whenever \code{installMiniconda} is re-run (and \code{BASILISK_USE_SYSTEM_DIR} is not set, see \code{?\link{getBasiliskDir}}),
#' the previous Miniconda installation and its various \pkg{basilisk} environments are destroyed.
#' This avoids duplication of Miniconda instances that would otherwise chew up disk space at 3 GB a pop.
#' 
#' After the destruction of the previous instance, we rely on the client packages to recreate their required environments.
#' They should do this automatically if they are using \pkg{basilisk} correctly.
#'
#' Users can disable this destruction by setting the \code{BASILISK_NO_DESTROY} environment variable to \code{"1"}.
#' This may be necessary on rare occasions when running multiple R instances on the same Bioconductor release.
#' (Setting this variable is not required for instances using different Bioconductor releases.)
#'
#' @return
#' An Miniconda instance is created at the location specified by \code{\link{getBasiliskDir}}.
#' Nothing is performed if the instance already exists.
#' A logical scalar is returned indicating whether a new instance was created.
#'  
#' @author Aaron Lun
#'
#' @examples
#' # We can't actually run installMiniconda() here, as it 
#' # either relies on basilisk already being installed or
#' # it has a hard-coded path to the basilisk system dir.
#' print("dummy test to pass BiocCheck")
#'
#' @export
installMiniconda <- function(installed=TRUE) {
    dest_path <- getBasiliskDir(installed=installed)
    lock_file <- getLockFile(dest_path)

    if (file.exists(dest_path)) {
        if (!file.exists(lock_file)) {
            return(FALSE)
        }

        warning(sprintf("replacing incomplete Miniconda installation at '%s'", dest_path))
        unlink(dest_path, recursive=TRUE, force=TRUE)
        unlink(lock_file, force=TRUE)
    }

    # If we're assuming that basilisk is installed, and we're using a system
    # directory, and the Miniconda installation directory is missing, something
    # is clearly wrong. We check this here instead of in `getBasiliskDir()` to
    # avoid throwing after an external install, given that `installMiniconda()`
    # is usually called before `getBasiliskDir()`.
    if (installed && useSystemDir()) {
        stop("Miniconda should have been installed during basilisk installation")
    }

    if (!useSystemDir() && destroyOldVersions()) {
        clearExternalDir()
    }
    if (!dir.create(dirname(dest_path), showWarnings=FALSE, recursive=TRUE)) {
        stop("failed to create '", dirname(dest_path), "'") 
    }
    write(file=lock_file, x=character(0))

    version <- "py37_4.8.2"
    base_url <- "https://repo.anaconda.com/miniconda"

    if (isWindows()) {
        arch <- if (.Machine$sizeof.pointer == 8) "x86_64" else "x86"
        inst_file <- sprintf("Miniconda3-%s-Windows-%s.exe", version, arch)
        #tmploc <- .expedient_download(file.path(base_url, inst_file))

        # I dunno, man. I'm just grasping at straws to avoid the Windows TIMEOUT.
        inst_file <- "Anaconda3-2019.10-Windows-x86_64.exe" 
        tmploc <- file.path("https://repo.anaconda.com/archive", inst_file)
        tmploc <- .expedient_download(tmploc)

        # Using the same code as reticulate:::miniconda_installer_run.
        dir.create(dest_path, recursive = TRUE, showWarnings = FALSE)
        inst_args <- c("/InstallationType=JustMe", "/AddToPath=0",
            "/RegisterPython=0", "/S", "/NoRegistry=1",
            sprintf("/D=%s", utils::shortPathName(dest_path)))
        Sys.chmod(tmploc, mode = "0755")
        status <- system2(tmploc, inst_args)

    } else {
        is_mac <- isMacOSX()
        sysname <- if (is_mac) "MacOSX" else "Linux"
        inst_file <- sprintf("Miniconda3-%s-%s-x86_64.sh", version, sysname)

        tmploc <- .expedient_download(file.path(base_url, inst_file))
        inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path)

        if (is_mac) {
            # The prebuilt R binary for Mac seems to set this variable,
            # which causes default paths for zlib to be ignored and breaks
            # installation. So, we unset it before attempting installation.
            status <- system(paste("unset DYLD_FALLBACK_LIBRARY_PATH; bash", inst_args))

            # Eliminating MKL from the installation, see https://github.com/rstudio/reticulate/issues/758.
            # This is done by following advice in https://docs.anaconda.com/mkl-optimizations/.
            conda.bin <- getCondaBinary(dest_path)
            system2(conda.bin, c("install", "--yes", "--no-update-deps", "nomkl"))
        } else {
            status <- system2("bash", inst_args)
        }
    }

    # Rigorous checks for proper installation, heavily inspired if not outright
    # copied from reticulate::install_miniconda.
    if (status != 0) {
        stop(sprintf("Miniconda installation failed with status code '%s'", status))
    }

    conda.exists <- file.exists(getCondaBinary(dest_path))
    python.cmd <- getPythonBinary(dest_path)
    report <- system2(python.cmd, c("-E", "-c", shQuote("print(1)")), stdout=TRUE, stderr=FALSE)
    if (!conda.exists || report!="1") {
        stop("Miniconda installation failed for an unknown reason")
    }

    unlink(lock_file, force=TRUE)
    TRUE 
}

#' @importFrom utils download.file
#' @importFrom methods is
.expedient_download <- function(url) {
    fname <- try({
        bfc <- BiocFileCache::BiocFileCache(ask=FALSE)
        BiocFileCache::bfcrpath(bfc, url) 
    }, silent=TRUE)

    if (is(fname, "try-error")) {
        tmploc <- file.path(tempdir(), basename(url))
        if (download.file(url, tmploc, mode="wb")) {
            stop("failed to download the Miniconda installer")
        }
        fname <- tmploc
    }

    fname
}
