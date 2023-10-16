test_that("items missing ", {
  expect_error(prob_scores(data = MaxDiff,
                           set.size = 4))
})

test_that("Only 1 item ", {
  expect_error(prob_scores(data = MaxDiff,
                           item = Option_01,
                           set.size = 4))
})

test_that("Item not numeric ", {
  data2 <- MaxDiff
  data2$Option_01 <- as.character(data2$Option_01)
  expect_error(prob_scores(data = data2,
                           items = c(Option_01:Option_16),
                           set.size = 4))
})

test_that("Group contains NAs - warning ", {
  data2 <- MaxDiff
  data2$Group[34] <- NA
  expect_warning(prob_scores(data = data2,
                           items = c(Option_01:Option_16),
                           set.size = 4,
                           group = Group))
})

test_that("Items no NAs ", {
  data2 <- MaxDiff
  data2$Option_01[34] <- NA
  expect_error(prob_scores(data = data2,
                             items = c(Option_01:Option_16),
                             set.size = 4))
})


test_that("set.size missing ", {
  expect_error(prob_scores(data = MaxDiff,
                           items = c(Option_01:Option_16)))
})

test_that("set.size not numeric ", {
  expect_error(prob_scores(data = MaxDiff,
                           items = c(Option_01:Option_16),
                           set.size = "a"))
})

test_that("set.size larger than items ", {
  expect_error(prob_scores(data = MaxDiff,
                           items = c(Option_01:Option_16),
                           set.size = 17))
})

test_that("anchor needs to be numeric ", {
  data2 <- MaxDiff
  data2$none <- as.character(data2$none)
  expect_error(prob_scores(data = data2,
                           items = c(Option_01:Option_16),
                           set.size = 4,
                           anchor = "none"))
})

test_that("anchor contains NAs ", {
  data2 <- MaxDiff
  data2$none[34] <- NA
  expect_error(prob_scores(data = data2,
                           items = c(Option_01:Option_16),
                           set.size = 4,
                           anchor = "none"))
})

