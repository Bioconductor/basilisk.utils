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

test_that("lockInstallation works as expected", {
    # Can't test for lockfile creation as BASILISK_USE_SYSTEM_DIR might be 1.
    loc <- lockInstallation()
    expect_null(unlockInstallation(loc))
})
