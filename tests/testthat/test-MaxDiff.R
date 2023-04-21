test_that("Structure of DF", {
  expect_equal(nrow(MaxDiff), 70)
  expect_equal(ncol(MaxDiff), 21)
})

test_that("Variables numeric",{
  names <- colnames(MaxDiff)[-(which(colnames(MaxDiff)=="Group"))]
  for (i in 1:length(names)){
    expect_true(!is.character(MaxDiff[names[i]]))
  }
})

test_that("No missings",{
  expect_false(anyNA(MaxDiff))
})

