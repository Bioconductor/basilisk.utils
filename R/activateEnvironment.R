#' Activate a Conda environment
#'
#' Mimic the (de)activation of a Conda environment by modifying environment variables in the current R process.
#'
#' @param envpath String containing the path to the Conda environment to activate.
#' If \code{NULL}, the base Conda instance at \code{\link{getCondaDir}()} is activated.
#' @param listing Named list of strings containing name:value pairs for environment variables,
#' typically the output of \code{activateEnvironment}.
#' @param loc String containing the path to the root of a conda instance. 
#'
#' @details
#' Conda environments generally need to be activated to function properly.
#' This is especially relevant on Windows where the \code{"PATH"} variable needs to be modified for the DLL search.
#' The \code{.activateEnvironment} function mimics the effect of activation
#' by modifying environment variables in the current R session.
#' This can be reversed by \code{.deactivateEnvironment} once the Conda environment is no longer in use.
#'
#' The \code{.activateEnvironment} function will also unset a few bothersome environment variables:
#' \itemize{
#' \item \code{"PYTHONPATH"}: to avoid compromising the version guarantees 
#' if \pkg{reticulate}'s \code{import} is allowed to search other locations beyond the specified Conda environment.
#' \item \code{"PYTHONNOUSERSITE"}: similarly, to avoid searching the user's site libraries.
#' \item \code{"RETICULATE_PYTHON"}: this would otherwise override any choice of Python, 
#' even after explicit specification via \pkg{reticulate}'s \code{use_Condaenv}!
#' \item \code{"RETICULATE_PYTHON_ENV"}: for similar reasons.
#' }
#'
#' @return
#' \code{activateEnvironment} will modify environment variables to mimic activation of the Conda environment.
#' It returns a named list of the previous values of all variables modified in this manner.
#' (\code{NA} values indicate that the corresponding variable was not previously set.)
#'
#' \code{deactivateEnvironment} restores the environment variables to their pre-activation state.
#' It returns \code{NULL} invisibly.
#' 
#' @examples
#' # We can't actually run activateEnvironment() here, as it 
#' # either relies on basilisk already being installed or
#' # it has a hard-coded path to the basilisk system dir.
#' print("dummy test to pass BiocCheck")
#'
#' @export
#' @author Aaron Lun
activateEnvironment <- function(envpath=NULL, loc=getCondaDir()) {
    ADD <- function(listing, var) {
        previous <- Sys.getenv(var, unset=NA)
        if (!is.na(previous)) {
            listing[[var]] <- previous
        }
        listing
    }

    output <- list()
    output <- ADD(output, "PYTHONPATH")
    Sys.unsetenv("PYTHONPATH")

    # This also needs to be unset otherwise it seems to take priority over
    # everything, even if you explicitly request to use a specific conda env's
    # Python (see LTLA/basilisk#1).
    output <- ADD(output, "RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")

    output <- ADD(output, "RETICULATE_PYTHON_ENV")
    Sys.unsetenv("RETICULATE_PYTHON_ENV")

    # Isolating from any user-specific site-packages, see conda/conda#394.
    output <- ADD(output, "PYTHONNOUSERSITE")
    Sys.setenv(PYTHONNOUSERSITE=1)

    # Activating the conda environment.
    output <- .activate_condaenv(output, envpath, loc)

    output
}

#' @importFrom methods is
.activate_condaenv <- function(listing, envpath, loc) {
    if (isWindows()) {
        act.bat <- file.path(loc, "condabin", "conda.bat")
        act.cmd <- c(shQuote(act.bat), "activate")
        if (!is.null(envpath)) {
            act.cmd <- c(act.cmd, shQuote(envpath))
        }
    } else {
        profile.sh <- file.path(loc, "etc", "profile.d", "conda.sh")
        act.cmd <- c(".", shQuote(profile.sh), "&&", "conda", "activate")
        if (!is.null(envpath)) {
            act.cmd <- c(act.cmd, shQuote(envpath))
        }
    }

    # Most of this is copied from parallel:::initDefaultClusterOptions.
    # We try several times to acquire the socket with different ports;
    # hopefully one of those will work.
    seed <- .GlobalEnv$.Random.seed
    on.exit({
        if (is.null(seed)) {
            rm(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        } else {
            assign(".Random.seed", seed, envir = .GlobalEnv, inherits = FALSE)
        }
    }, add=TRUE)

    success <- FALSE 
    for (tries in seq_len(10)) {
        ran1 <- sample.int(.Machine$integer.max - 1L, 1L)/.Machine$integer.max
        p <- as.integer(11000 + 1000 * ((ran1 + unclass(Sys.time())/300)%%1))
        soc <- try(serverSocket(p), silent=TRUE) # should fail if the socket is already in use.
        if (!is(soc, "try-error")) {
            on.exit(close(soc), add=TRUE)
            success <- TRUE
            break;
        }
    }

    if (success) {
        # Identifying all environment variables after activation.
        con.cmd <- paste(sprintf("con <- socketConnection(port=%i, open='wb', blocking=TRUE)", p),
            "serialize(Sys.getenv(), con)", "close(con)", sep=";")
        act.cmd <- c(act.cmd, "&&", file.path(R.home("bin"), "Rscript"), 
            "--no-save", "--no-restore", "--no-site-file", "--no-init-file",
            "--default-packages=NULL", "-e", deparse(con.cmd))

        out <- system(paste(act.cmd, collapse=" "), intern=TRUE)
        status <- attr(out, "status")

        if (is.null(status) || isTRUE(status == 0)) {
            listener <- try(socketAccept(soc, blocking=TRUE, open = "a+b"), silent=TRUE)
            success <- !is(listener, "try-error")
            if (success) {
                on.exit(close(listener), add=TRUE)
            }
        }
    }

    if (!success) {
        warning("failed to activate the environment at '", envpath, "'")
        return(list())
    }

    actvar <- unserialize(listener)
    extvar <- Sys.getenv()

    if (isWindows()) {
        # Case insensitive on Windows. Hey, I don't make the rules.
        names(extvar) <- toupper(names(extvar))
        names(actvar) <- toupper(names(actvar))
    }

    # Manually applying changes to the environment variables while recording 
    # their previous state so that we can unset them appropriately.
    needs.setting <- setdiff(names(actvar), names(extvar))
    for (i in needs.setting) {
        listing[[i]] <- NA
    }

    needs.replacing <- intersect(names(extvar), names(actvar))
    needs.replacing <- needs.replacing[extvar[needs.replacing]!=actvar[needs.replacing]]
    for (i in needs.replacing) {
        listing[[i]] <- extvar[[i]]
    }

    to.change <- union(needs.replacing, needs.setting)
    if (length(to.change)) {
        do.call(Sys.setenv, as.list(actvar[to.change]))
    }

    needs.unsetting <- setdiff(names(extvar), names(actvar))
    for (i in needs.unsetting) {
        listing[[i]] <- extvar[[i]]
    }
    Sys.unsetenv(needs.unsetting)

    listing
}

#' @export
#' @rdname activateEnvironment
deactivateEnvironment <- function(listing) {
    for (x in names(listing)) {
        if (is.na(listing[[x]])) {
            Sys.unsetenv(x)
        } else {
            do.call(Sys.setenv, listing[x])
        }
    }
    invisible(NULL)
}
