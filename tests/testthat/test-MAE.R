library(validateHOT)
####################### Test wo Grouping variable ########################################

HOT <- createHOT(
  data = MaxDiff, None = 19,
  id = 1, prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff"
)

test_that("Structure of Output", {
  expect_true(base::is.data.frame(mae(data = HOT, id = 1, opts = c(2:9), choice = 10)))
})

test_that("Structure of Output", {
  expect_equal(base::nrow(mae(data = HOT, id = 1, opts = c(2:9), choice = 10)), 1)
  expect_equal(base::ncol(mae(data = HOT, id = 1, opts = c(2:9), choice = 10)), 1)
})

test_that("Labeling correct", {
  expect_equal(base::colnames(mae(data = HOT, id = 1, opts = c(2:9), choice = 10)), "MAE")
})

test_that("Count of correct predicted people", {
  expect_equal(base::round(mae(data = HOT, id = 1, opts = c(2:9), choice = 10)[1, 1], digits = 3), 4.933)
})

test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- base::as.character(HOT2$choice)
  expect_error(mae(data = HO2, id = 1, opts = c(2:9), choice = 10))
})

test_that("Wrong format Option", {
  HOT2 <- HOT
  HOT2$Option_2 <- base::as.character(HOT2$Option_2)
  expect_error(mae(data = HO2, id = 1, opts = c(2:9), choice = 10))
})

test_that("Test plausability of results", {
  expect_true(mae(data = HOT, id = 1, opts = c(2:9), choice = 10)[1, 1] >= 0)
})


test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(mae(data = HOT2, id = 1, opts = c(2:9), choice = 10))
})

test_that("No missings in output", {
  expect_false(base::anyNA(mae(data = HOT, id = 1, opts = c(2:9), choice = 10)))
})


test_that("mae() also working with data.frame not created with createHOT()", {
  newHOT <- base::data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:4), 10, replace = T)
  )

  expect_equal(base::nrow(mae(data = newHOT, id = 1, opts = c(2:5), choice = 6)), 1)
  expect_equal(base::ncol(mae(data = newHOT, id = 1, opts = c(2:5), choice = 6)), 1)

  expect_false(base::anyNA(mae(data = newHOT, id = 1, opts = c(2:5), choice = 6)))
})


####################### Test with Grouping variable ########################################

HOT <- createHOT(
  data = MaxDiff, None = 19,
  id = 1, prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Structure of Output", {
  expect_true(base::is.data.frame(mae(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)))
})

test_that("Expect warning if Grouping variable has NAs", {
  HOT2 <- HOT
  HOT2$Group[c(10, 20, 30)] <- NA
  expect_warning(mae(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})

test_that("Structure of Output", {
  expect_equal(base::nrow(mae(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), (base::length(base::unique(HOT$Group)) + 1))
  expect_equal(base::ncol(mae(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), 2)
})

test_that("Labeling correct", {
  expect_equal(base::colnames(mae(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), c("Group", "MAE"))
})

test_that("Test plausability of results", {
  Results <- mae(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)

  for (i in 1:nrow(Results)) {
    expect_true(Results[i, 2] >= 0)
  }
})



test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- base::as.character(HOT2$choice)
  expect_error(mae(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})

test_that("Wrong format Option", {
  HOT2 <- HOT
  HOT2$Option_2 <- base::as.character(HOT2$Option_2)
  expect_error(mae(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})


test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(mae(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})

test_that("No missings in output", {
  expect_false(base::anyNA(mae(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)))
})


test_that("mae() also working with data.frame not created with createHOT()", {
  newHOT <- base::data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:4), 10, replace = T),
    Group = base::sample(c(1, 2), 10, replace = T)
  )

  expect_equal(base::nrow(mae(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)), (base::length(base::unique(newHOT$Group)) + 1))
  expect_equal(base::ncol(mae(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)), 2)

  expect_false(base::anyNA(mae(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)))
})


test_that("Right labels of 'Group' variable", {
  # Factor

  HOT2 <- HOT

  ## change 'Group' to factor

  HOT2$Group <- base::factor(HOT2$Group,
    levels = c(1:3),
    labels = c("Group 1", "Group 2", "Group 3")
  )

  lev <- c(base::levels(HOT2$Group))

  Results <- mae(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])



  # Labelled data

  HOT2 <- HOT

  ## change 'Group' to labelled data
  HOT2$Group <- labelled::labelled(HOT2$Group,
    labels = c("Group 1" = 1, "Group 2" = 2, "Group 3" = 3)
  )
  labelled::val_labels(HOT2$Group, prefixed = T)

  lev <- c(base::names(labelled::val_labels(HOT2$Group)))

  Results <- mae(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])


  # character
  HOT2 <- HOT

  ## change 'Group' to character
  HOT2$Group <- base::as.character(HOT2$Group)

  lev <- c(base::sort(base::unique(HOT2$Group)))

  Results <- mae(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])
})
