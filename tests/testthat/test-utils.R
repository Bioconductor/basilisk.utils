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
    tmp <- file.path(tempfile(), "1.0.0")

    loc <- lockExternalDir(tmp)
    expect_true(file.exists(paste0(tmp, "-00LOCK")))
    expect_error(lockExternalDir(tmp, exclusive=FALSE), "exclusive")
    expect_null(unlockExternalDir(loc))

    loc1 <- lockExternalDir(tmp, exclusive=FALSE)
    loc2 <- lockExternalDir(tmp, exclusive=FALSE)
    expect_error(lockExternalDir(tmp, exclusive=TRUE), "shared")
    unlockExternalDir(loc1)
    unlockExternalDir(loc2)
})
