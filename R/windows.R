# Because on Windows, life is suffering due to the 260 character file path
# limit. We still want to keep everything in the same external directory, to
# avoid name conflicts with other programs that use the cache. To do so, we
# give short integer names for the directories and use files with longer names
# to reference those directories. We limit this to Windows to avoid the cost
# and complexity of all of this reading/writing to disk for unaffected OS's.

.retrieve_win_reference <- function(pkgname, version) {
    exdir <- getExternalDir()
    dest <- paste0(pkgname, "-", version)

    target <- file.path(exdir, dest)
    if (file.exists(target)) {
        return(file.path(exdir, readLines(target)))
    }

    # Autoincrementing. We should always be guaranteed the base
    # installation at '0', but you never know, so I'll just 
    # program defensively against that.
    existing <- list.dirs(exdir, full.names=FALSE, recursive=FALSE)

    if (length(existing)) {
        existing <- as.integer(existing)
        maxed <- max(existing, na.rm=TRUE)
        remnants <- setdiff(seq_len(maxed), existing)
        if (length(remnants)) {
            chosen <- remnants[1]
        } else {
            chosen <- maxed + 1L
        }
    } else {
        chosen <- 1L
    }

    output <- file.path(exdir, chosen)
    dir.create(output, recursive=TRUE)
    write(file=target, chosen)

    output
}

.flush_win_references <- function(paths) {
    for (p in paths) {
        if (file.exists(p) && !dir.exists(p)) {
            target <- file.path(dirname(p), readLines(p))
            unlink(target, recursive=TRUE)
        }
    }
    invisible(NULL)
}

.test_win_references <- function() {
    # Strictly for testing only.
    Sys.getenv("BASILISK_TEST_REFERENCES")!=""
}
