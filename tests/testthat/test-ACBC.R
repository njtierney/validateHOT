test_that("Structure of DF", {
  expect_equal(nrow(ACBC), 80)
  expect_equal(ncol(ACBC), 34)
})

test_that("Variables numeric",{
  names <- colnames(ACBC)[-(which(colnames(ACBC)=="Group"))]
  for (i in 1:length(names)){
    expect_true(!is.character(ACBC[names[i]]))
  }
})

test_that("No missings",{
  expect_false(anyNA(ACBC))
})
