library(ValiDatHOT)
data(MaxDiff)

####################### Test wo Grouping variable ########################################
createHOT(
  data = MaxDiff, None = 19,
  id = 1, prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff"
)

test_that("Structure of Output", {
  expect_true(base::is.data.frame(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")))
  expect_true(base::is.data.frame(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")))
})

test_that("Right method", {
  expect_error(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "test"))
})

test_that("Structure of Output", {
  expect_equal(base::nrow(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")), 1)
  expect_equal(base::ncol(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")), 1)

  expect_equal(base::nrow(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")), 1)
  expect_equal(base::ncol(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")), 1)
})

test_that("Labeling correct", {
  expect_equal(base::colnames(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")), "Frequency")
  expect_equal(base::colnames(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")), "Frequency")
})


test_that("Test plausability of results", {
  expect_true(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")[1, 1] <= 3)
  expect_true(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")[1, 1] <= 1)
})

test_that("Make sure test data is correct", {
  expect_equal(base::round(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")[1, 1], digits = 3), 1.443)
  expect_equal(base::round(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")[1, 1], digits = 3), 0.700)
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
  expect_error(freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold"))
  expect_error(freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice"))
})

test_that("No missings in output", {
  expect_false(base::anyNA(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")))
  expect_false(base::anyNA(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")))
})

test_that("freqassort() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T)
  )

  expect_equal(base::nrow(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, method = "threshold")), 1)
  expect_equal(base::ncol(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, method = "threshold")), 1)

  expect_false(base::anyNA(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, method = "threshold")))

  expect_equal(base::nrow(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, method = "First Choice")), 1)
  expect_equal(base::ncol(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, method = "First Choice")), 1)

  expect_false(base::anyNA(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, method = "First Choice")))
})

####################### Test with Grouping variable ########################################

createHOT(
  data = MaxDiff, None = 19,
  id = 1, prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Structure of Output", {
  expect_true(base::is.data.frame(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")))
  expect_true(base::is.data.frame(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")))
})

test_that("Structure of Output", {
  expect_equal(base::nrow(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")), (base::length(base::unique(HOT$Group)) + 1))
  expect_equal(base::ncol(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")), 2)

  expect_equal(base::nrow(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")), (base::length(base::unique(HOT$Group)) + 1))
  expect_equal(base::ncol(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")), 2)
})

test_that("Labeling correct", {
  expect_equal(base::colnames(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")), c("Group", "Frequency"))
  expect_equal(base::colnames(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")), c("Group", "Frequency"))
})


test_that("Test plausability of results", {
  Results <- freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")

  for (i in 1:base::nrow(Results)) {
    expect_true(Results[i, 2] <= 3)
  }

  Results <- freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")

  for (i in 1:base::nrow(Results)) {
    expect_true(Results[i, 2] <= 1)
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
  expect_error(freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold"))
  expect_error(freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice"))
})

test_that("No missings in output", {
  expect_false(base::anyNA(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")))
  expect_false(base::anyNA(freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")))
})

test_that("freqassort() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T),
    Group = base::sample(c(1, 2), 10, replace = T)
  )

  expect_equal(base::nrow(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, Group = 8, method = "threshold")), (base::length(base::unique(newHOT$Group)) + 1))
  expect_equal(base::ncol(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, Group = 8, method = "threshold")), 2)

  expect_false(base::anyNA(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, Group = 8, method = "threshold")))

  expect_equal(base::nrow(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, Group = 8, method = "First Choice")), (base::length(base::unique(newHOT$Group)) + 1))
  expect_equal(base::ncol(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, Group = 8, method = "First Choice")), 2)

  expect_false(base::anyNA(freqassort(data = newHOT, id = 1, bundles = c(2, 3, 5), None = 6, Group = 8, method = "First Choice")))
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

  Results <- freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])

  Results <- freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")


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

  Results <- freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])

  Results <- freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])


  # character
  HOT2 <- HOT

  ## change 'Group' to character
  HOT2$Group <- base::as.character(HOT2$Group)

  lev <- c(base::sort(base::unique(HOT2$Group)))

  Results <- freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "threshold")


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])

  Results <- freqassort(data = HOT2, id = 1, bundles = c(2, 3, 7), None = 9, Group = 10, method = "First Choice")


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])
})
