library(ValiDatHOT)
data("MaxDiff")

test_that("wrong input", {
  expect_error(createHOT(data = MaxDiff,
                         id = "id",
                         prod = 7,
                         prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                         choice = 20,
                         method = "MaxDiff"))
  })

test_that("wrong input", {
  expect_error(createHOT(data = MaxDiff,
                         id = 1,
                         None = "none",
                         prod = 7,
                         prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                         choice = 20,
                         method = "MaxDiff"))
})

test_that("With None one columns more", {
  expect_equal((base::length(createHOT(data = MaxDiff,
                      id = 1,
                      None = 19,
                      prod = 7,
                      prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                      choice = 20,
                      method = "MaxDiff")) -
                base::length(createHOT(data = MaxDiff,
                        id = 1,
                        prod = 7,
                        prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                        choice = 20,
                        method = "MaxDiff"))
  ), 1)
})

test_that("Coding not needed for MaxDiff", {
  expect_error(createHOT(data = MaxDiff,
                         id = 1,
                         None = 19,
                         prod = 7,
                         prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                         choice = 20,
                         coding = rep(0, 7),
                         method = "MaxDiff"))
})

test_that("interpolate.leves not needed for MaxDiff", {
  expect_error(createHOT(data = MaxDiff,
                         id = 1,
                         None = 19,
                         prod = 7,
                         prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                         choice = 20,
                         interpolate.levels = list(c(20:30)),
                         method = "MaxDiff"))
})

test_that("piece.p not needed for MaxDiff", {
  expect_error(createHOT(data = MaxDiff,
                         id = 1,
                         None = 19,
                         prod = 7,
                         prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                         choice = 20,
                         piece.p = list(c(14,15)),
                         method = "MaxDiff"))
})

test_that("lin.p not needed for MaxDiff", {
  expect_error(createHOT(data = MaxDiff,
                         id = 1,
                         None = 19,
                         prod = 7,
                         prod.levels = list(3, 10, 11, 15, 16, 17, 18),
                         choice = 20,
                         lin.p = c(14,15),
                         method = "MaxDiff"))
})


test_that("prod.levels not a list", {
  expect_error(createHOT(data = MaxDiff,
                         id = 1,
                         None = 19,
                         prod = 7,
                         prod.levels = c(3, 10, 11, 15, 16, 17, 18),
                         choice = 20,
                         method = "MaxDiff"))
})

