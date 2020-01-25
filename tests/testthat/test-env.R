# Checking that everyone responds to their environment variables.
# library(testthat); library(basilisk.utils); source("test-env.R")

test_that("BASILISK_EXTERNAL_ANACONDA works", {
    Sys.setenv(BASILISK_EXTERNAL_ANACONDA="blah")
    expect_identical('blah', getBasiliskDir(assume.installed=FALSE))
    Sys.unsetenv("BASILISK_EXTERNAL_ANACONDA")
})

test_that("BASILISK_USE_SYSTEM_DIR works", {
    Sys.setenv(BASILISK_USE_SYSTEM_DIR="1")
    
    out <- getBasiliskDir(assume.installed=FALSE)
    expect_identical(basename(out), "anaconda")

    out <- getEnvironmentDir("whee", assume.installed=FALSE)
    expect_identical(basename(out), "basilisk")
    expect_identical(basename(dirname(out)), "whee")

    Sys.unsetenv("BASILISK_USE_SYSTEM_DIR")
})

test_that("BASILISK_EXTERNAL_DIR works", {
    # Unfortunately, there is no path here that is possible
    # without a basilisk installation. So we must skip this.
})
