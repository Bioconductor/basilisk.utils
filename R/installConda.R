#' Install (Mini)conda 
#'
#' Install conda - usually Miniconda, sometimes Anaconda - to an appropriate destination path,
#' skipping the installation if said path already exists.
#'
#' @param installed Logical scalar indicating whether \pkg{basilisk} is already installed.
#' Should only be set to \code{FALSE} in \pkg{basilisk} \code{configure} scripts.
#' 
#' @details
#' This function was originally created from code in \url{https://github.com/hafen/rminiconda},
#' also borrowing code from \pkg{reticulate}'s \code{install_miniconda} for correct Windows installation.
#' It downloads and runs an appropriate conda installer to create a conda instance for use by \pkg{basilisk}.
#' We use \pkg{BiocFileCache} if available to avoid redownloading the installer upon \pkg{basilisk} re-installation.
#'
#' Currently, the type and version of installer depends on the operating system:
#' \itemize{
#' \item On Linux, we use version 4.8.2 of the Miniconda3 installer.
#' \item On MacOS, we use version 4.8.2 of the Miniconda3 installer, 
#' followed by installation of \pkg{nomkl} package.
#' This is necessary to avoid issues with Mojave notarization,
#' see \url{https://github.com/rstudio/reticulate/issues/758}.
#' \item On Windows, we use version 2019.10 of the Anaconda3 installer,
#' as other versions (and Miniconda) cause TIMEOUTs on the Bioconductor build system.
#' The reason for this behavior is unknown.
#' }
#'
#' @section Destruction of old instances:
#' Whenever \code{installConda} is re-run (and \code{BASILISK_USE_SYSTEM_DIR} is not set, see \code{?\link{getCondaDir}}),
#' any previous conda instances and their associated \pkg{basilisk} environments are destroyed.
#' This avoids duplication of large conda instances after their obselescence.
#' Client packages are expected to recreate their environments in the latest conda instance.
#'
#' Users can disable this destruction by setting the \code{BASILISK_NO_DESTROY} environment variable to \code{"1"}.
#' This may be necessary on rare occasions when running multiple R instances on the same Bioconductor release.
#' Note that setting this variable is not required for R instances using different Bioconductor releases;
#' the destruction is smart enough to only remove conda instances generated from the same release.
#'
#' @return
#' A conda instance is created at the location specified by \code{\link{getCondaDir}}.
#' Nothing is performed if a complete instance already exists at that location.
#' A logical scalar is returned indicating whether a new instance was created.
#'  
#' @author Aaron Lun
#'
#' @examples
#' # We can't actually run installConda() here, as it 
#' # either relies on basilisk already being installed or
#' # it has a hard-coded path to the basilisk system dir.
#' print("dummy test to pass BiocCheck")
#'
#' @export
installConda <- function(installed=TRUE) {
    if (!is.na(.get_external_conda())) {
        return(FALSE)
    }

    dest_path <- getCondaDir(installed=installed)

    if (!useSystemDir()) {
        # Locking the installation; this ensures we will wait for any
        # concurrently running installations to finish. 
        loc <- lockExternalDir(exclusive=!file.exists(dest_path))
        on.exit(unlockExternalDir(loc))
    }

    # Do NOT assign the existence of dest_path to a variable for re-use in the
    # locking call above. We want to recheck existance just in case the
    # directory was created after waiting to acquire the lock.
    if (file.exists(dest_path)) {
        return(FALSE)
    }

    # If we're assuming that basilisk is installed, and we're using a system
    # directory, and the conda installation directory is missing, something
    # is clearly wrong. We check this here instead of in `getCondaDir()` to
    # avoid throwing after an external install, given that `installConda()`
    # is usually called before `getCondaDir()`.
    if (installed && useSystemDir()) {
        stop("conda should have been installed during basilisk installation")
    }

    # We need to wipe out Conda installations for older versions of basilisk;
    # we also need to destroy any stale installations and environments for the
    # current version.
    if (!useSystemDir() && destroyOldVersions()) {
        clearExternalDir()
    }

    host <- dirname(dest_path)
    unlink2(host) 
    dir.create2(host)

    # Destroying the directory upon failure, to avoid difficult interpretations
    # of any remnant installation directories when this function is hit again.
    success <- FALSE
    on.exit(if (!success) unlink2(dest_path, recursive=TRUE), add=TRUE, after=FALSE)

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
        dir.create2(dest_path)
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
        stop(sprintf("conda installation failed with status code '%s'", status))
    }

    conda.exists <- file.exists(getCondaBinary(dest_path))
    if (conda.exists && isWindows()) {
        # Sometimes Windows doesn't create this file. Why? WHO KNOWS.
        conda.exists <- file.exists(file.path(dest_path, "condabin/conda.bat"))
    }

    python.cmd <- getPythonBinary(dest_path)
    report <- system2(python.cmd, c("-E", "-c", shQuote("print(1)")), stdout=TRUE, stderr=FALSE)
    if (!conda.exists || report!="1") {
        stop("conda installation failed for an unknown reason")
    }

    success <- TRUE
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
            stop("failed to download the conda installer")
        }
        fname <- tmploc
    }

    fname
}
