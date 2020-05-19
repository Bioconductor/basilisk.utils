# Checking that everyone responds to their environment variables.
# library(testthat); library(basilisk.utils); source("test-env.R")

test_that("BASILISK_EXTERNAL_CONDA works", {
    Sys.setenv(BASILISK_EXTERNAL_CONDA="blah")
    expect_identical('blah', getBasiliskDir(installed=FALSE))
    Sys.unsetenv("BASILISK_EXTERNAL_CONDA")
})

test_that("BASILISK_USE_SYSTEM_DIR works", {
    Sys.setenv(BASILISK_USE_SYSTEM_DIR="1")
    
    out <- getBasiliskDir(installed=FALSE)
    expect_identical(basename(out), "conda")

    out <- getEnvironmentDir("whee", installed=FALSE)
    expect_identical(basename(out), "basilisk")
    expect_identical(basename(dirname(out)), "whee")

    Sys.unsetenv("BASILISK_USE_SYSTEM_DIR")
})

test_that("BASILISK_EXTERNAL_DIR works", {
    # Unfortunately, there is no path here that is possible
    # without a basilisk installation. So we must skip this.
})
