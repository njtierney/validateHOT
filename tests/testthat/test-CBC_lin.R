library(ValiDatHOT)
data(CBC_lin)

test_that("Structure of DF", {
  expect_equal(base::nrow(CBC_lin), 79)
  expect_equal(base::ncol(CBC_lin), 17)
})

test_that("Variables numeric", {
  names <- base::colnames(CBC_lin)[-(base::which(base::colnames(CBC_lin) == "Group"))]
  for (i in 1:base::length(names)) {
    expect_true(!base::is.character(CBC_lin[names[i]]))
  }
})

test_that("No missings", {
  expect_false(base::anyNA(CBC_lin))
})
