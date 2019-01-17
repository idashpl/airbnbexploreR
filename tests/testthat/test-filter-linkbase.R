context("Filter linkbase")

test_that("filter_linkbase returns some data", {
    expect_equal(nrow(filter_linkbase(format = "raw", city = "London")), 3)
    expect_equal(nrow(filter_linkbase(data_to_load = "listings", format = "raw", city = "London")), 1)
    expect_equal(filter_linkbase(archived = F) %>%
                     pull(archived) %>%
                     n_distinct(), 1)
})
