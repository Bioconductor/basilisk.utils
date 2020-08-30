# Checking that everyone responds to their environment variables.
# library(testthat); library(basilisk.utils); source("test-env.R")

test_that("BASILISK_EXTERNAL_CONDA works", {
    old <- setVariable("BASILISK_EXTERNAL_CONDA", "blah")

    expect_identical('blah', getCondaDir(installed=FALSE))

    expect_false(installConda())

    setVariable("BASILISK_EXTERNAL_CONDA", old)
})

test_that("BASILISK_USE_SYSTEM_DIR works", {
    old <- setVariable("BASILISK_USE_SYSTEM_DIR", NA)
    expect_false(useSystemDir())

    setVariable("BASILISK_USE_SYSTEM_DIR", "1")
    expect_true(useSystemDir())
    
    out <- getCondaDir(installed=FALSE)
    expect_identical(basename(out), "conda")

    setVariable("BASILISK_USE_SYSTEM_DIR", old)
})

test_that("BASILISK_NO_DESTROY works", {
    old <- setVariable("BASILISK_NO_DESTROY", NA)
    expect_true(destroyOldVersions())

    setVariable("BASILISK_NO_DESTROY", "1")
    expect_false(destroyOldVersions())

    setVariable("BASILISK_NO_DESTROY", old)
})

test_that("BASILISK_EXTERNAL_DIR works", {
    # Unfortunately, there is no path here that is possible
    # without a basilisk installation. So we must skip this.
})
