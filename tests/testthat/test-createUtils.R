context("Testing the functionality of create Utils")

test_that("Checking whether non-numeric content gives error",{

  expect_error(createUtil(is.character(id)|is.character(None)|is.character(None)))

})

test_that("Checking length of products is the same as list length", {
  expect_error(createUtil(length(x) != prod))
})

test_that("Checking ", {
  expect_error(createUtil(method == "ACBC", is.null(price)))
})
