# This tests the various utilities.
# library(testthat); library(basilisk.utils); source("test-utils.R")

test_that("special unlink2 works as expected", {
    tmp <- tempfile()
    write(file=tmp, character(0))
    expect_true(file.exists(tmp))
    unlink2(tmp)
    expect_false(file.exists(tmp))

    tmp <- tempfile()
    dir.create(tmp)
    expect_error(unlink2(tmp, recursive=FALSE), "failed to remove")
})

test_that("special dir.create2 works as expected", {
    tmp <- tempfile()
    dir.create2(tmp)
    expect_true(dir.exists(tmp))
    expect_error(dir.create2(tmp), 'failed to create')
})

test_that("lockExternalDir works as expected", {
    old <- Sys.getenv("BASILISK_EXTERNAL_DIR", NA)
    tmp <- tempfile()
    Sys.setenv(BASILISK_EXTERNAL_DIR=tmp)

    loc <- lockExternalDir()
    expect_true(file.exists(file.path(tmp, "00LOCK")))
    expect_error(lockExternalDir(exclusive=FALSE), "exclusive")
    expect_null(unlockExternalDir(loc))

    loc1 <- lockExternalDir(exclusive=FALSE)
    loc2 <- lockExternalDir(exclusive=FALSE)
    expect_error(lockExternalDir(exclusive=TRUE), "shared")
    unlockExternalDir(loc1)
    unlockExternalDir(loc2)

    if (is.na(old)) {
        Sys.unsetenv("BASILISK_EXTERNAL_DIR") 
    } else {
        Sys.setenv(BASILISK_EXTERNAL_DIR=old)
    }
})
