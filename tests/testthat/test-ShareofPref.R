library(validateHOT)
data(MaxDiff)

####################### Test wo Grouping variable ########################################
createHOT(
  data = MaxDiff, None = 19,
  id = 1, prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff"
)

test_that("Columns needs to be numeric", {
  HOT2 <- HOT
  HOT2$Option_2 <- as.character(HOT2$Option_2)
  expect_error(shareofpref(data = HOT2, id = 1, opts = c(2:9)))
})

test_that("Structure of Output", {
  expect_true(base::is.data.frame(shareofpref(data = HOT, id = 1, opts = c(2:9))))
})


test_that("Structure of Output", {
  expect_equal(base::nrow(shareofpref(data = HOT, id = 1, opts = c(2:9))), 8)

  expect_equal(base::ncol(shareofpref(data = HOT, id = 1, opts = c(2:9))), 4)
})

test_that("Labeling correct", {
  expect_equal(colnames(shareofpref(data = HOT, id = 1, opts = c(2:9))), c("Options", "Mean", "Lower CI", "Upper CI"))
})


test_that("Test plausability of results", {
  Results <- shareofpref(data = HOT, id = 1, opts = c(2:9))

  for (i in 1:nrow(Results)) {
    expect_true(Results[i, 2] <= 100)
    expect_true(Results[i, 3] < Results[i, 4] & Results[i, 3] < Results[i, 2])
    expect_true(Results[i, 4] > Results[i, 3])
  }

  expect_equal(base::sum(Results[, 2]), 100)
})


test_that("Make sure test data is correct", {
  expect_equal(base::round(shareofpref(data = HOT, id = 1, opts = c(2:9))[1, 2], digits = 3), 18.268)
  expect_equal(base::round(shareofpref(data = HOT, id = 1, opts = c(2:9))[1, 3], digits = 3), 10.189)
  expect_equal(base::round(shareofpref(data = HOT, id = 1, opts = c(2:9))[1, 4], digits = 3), 26.346)
})


test_that("Wrong format Option", {
  names <- base::colnames(HOT)[c(2:9)]

  for (i in 1:base::length(names)) {
    expect_true(base::is.numeric(HOT[[names[i]]]))
  }
})

test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 2] <- NA
  expect_error(shareofpref(data = HOT2, id = 1, opts = c(2:9)))
})

test_that("No missings in output", {
  expect_false(base::anyNA(shareofpref(data = HOT, id = 1, opts = c(2:9))))
})

test_that("shareofpref() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5)
  )

  expect_equal(base::nrow(shareofpref(data = newHOT, id = 1, opts = c(2:6))), (base::length(newHOT) - 1))
  expect_equal(base::ncol(shareofpref(data = newHOT, id = 1, opts = c(2:6))), 4)
})

####################### Test with Grouping variable ########################################

createHOT(
  data = MaxDiff, None = 19,
  id = 1, prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Structure of Output", {
  expect_true(base::is.list(shareofpref(data = HOT, id = 1, opts = c(2:9), Group = 10)))
})


test_that("Expect warning if Grouping variable has NAs", {
  HOT2 <- HOT
  HOT2$Group[c(10, 20, 30)] <- NA
  expect_warning(shareofpref(data = HOT2, id = 1, Group = 10, opts = c(2:9)))
})

test_that("Structure of Output", {
  expect_equal(base::length(shareofpref(data = HOT, id = 1, opts = c(2:9), Group = 10)), (base::length(base::unique(HOT$Group)) + 1))
})

test_that("Labeling correct", {
  for (i in 1:4) {
    expect_equal(base::colnames(shareofpref(data = HOT, id = 1, opts = c(2:9), Group = 10)[[i]]), c("Options", "Mean", "Lower CI", "Upper CI"))
  }
})


test_that("Test plausability of results", {
  for (i in 1:4) {
    Results <- shareofpref(data = HOT, id = 1, opts = c(2:9), Group = 10)[[i]]

    expect_equal(base::sum(Results[, 2]), 100)

    for (row in 1:8) {
      expect_true(Results[row, 3] < Results[row, 4] & Results[row, 3] < Results[row, 2])
      expect_true(Results[row, 4] > Results[row, 2])
    }
  }
})


test_that("Wrong format Option", {
  names <- base::colnames(HOT)[c(2:9)]

  for (i in 1:base::length(names)) {
    expect_true(base::is.numeric(HOT[[names[i]]]))
  }
})

test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 2] <- NA
  expect_error(shareofpref(data = HOT2, id = 1, opts = c(2:9), Group = 10))
})

test_that("No missings in output", {
  for (i in 1:4) {
    expect_false(base::anyNA(shareofpref(data = HOT, id = 1, opts = c(2:9), Group = 10)[[i]]))
  }
})

test_that("shareofpref() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    ID = c(1:100),
    Option_1 = stats::runif(100, min = -5, max = 5),
    Option_2 = stats::runif(100, min = -5, max = 5),
    Option_3 = stats::runif(100, min = -5, max = 5),
    Option_4 = stats::runif(100, min = -5, max = 5),
    Option_5 = stats::runif(100, min = -5, max = 5),
    Group = base::sample(c(1, 2), 100, replace = T)
  )

  for (i in 1:base::length(base::unique(newHOT$Group) + 1)) {
    expect_equal(base::nrow(shareofpref(data = newHOT, id = 1, opts = c(2:5), Group = 7)[[i]]), 4)
    expect_equal(base::ncol(shareofpref(data = newHOT, id = 1, opts = c(2:5), Group = 7)[[i]]), 4)

    expect_equal(base::length(shareofpref(data = newHOT, id = 1, opts = c(2:5), Group = 7)), (base::length(base::unique(newHOT$Group)) + 1))

    expect_false(base::anyNA(shareofpref(data = newHOT, id = 1, opts = c(2:5), Group = 7)[[i]]))
  }
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

  expect_equal(names(shareofpref(data = HOT2, id = 1, opts = c(2:9), Group = 10)), c("All", lev))


  # Labelled data

  HOT2 <- HOT

  ## change 'Group' to labelled data
  HOT2$Group <- labelled::labelled(HOT2$Group,
    labels = c("Group 1" = 1, "Group 2" = 2, "Group 3" = 3)
  )
  labelled::val_labels(HOT2$Group, prefixed = T)

  lev <- c(base::names(labelled::val_labels(HOT2$Group)))

  expect_equal(names(shareofpref(data = HOT2, id = 1, opts = c(2:9), Group = 10)), c("All", lev))

  # character
  HOT2 <- HOT

  ## change 'Group' to character
  HOT2$Group <- base::as.character(HOT2$Group)

  lev <- c(base::sort(base::unique(HOT2$Group)))

  expect_equal(names(shareofpref(data = HOT2, id = 1, opts = c(2:9), Group = 10)), c("All", lev))
})
