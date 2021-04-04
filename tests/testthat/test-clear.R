# library(testthat); library(basilisk.utils); source("test-clear.R")

test_that("clearing out only obsolete versions works as expected", {
    tmp <- tempfile()
    dir.create(tmp)

    version <- "0.0.1"
    placeholder <- file.path(tmp, version)
    dir.expiry::touchDirectory(placeholder)
    dir.create(placeholder, showWarnings=FALSE)
    expect_true(file.exists(placeholder))

    version2 <- "0.0.2"
    placeholder2 <- file.path(tmp, version2)
    dir.expiry::touchDirectory(placeholder2)
    dir.create(placeholder2, showWarnings=FALSE)
    expect_true(file.exists(placeholder2))

    clearObsoleteDir(path=placeholder2)
    expect_false(file.exists(placeholder))
    expect_true(file.exists(placeholder2))

    clearExternalDir(path=placeholder2)
    expect_false(file.exists(placeholder2))
})
