context("Build linkbase")

test_that("returned linkbase is not empty", {
    expect_false(nrow(build_linkbase()) == 0)
})
