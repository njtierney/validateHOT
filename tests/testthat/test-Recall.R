library(ValiDatHOT)
data(MaxDiff)

####################### Test wo Grouping variable ########################################
createHOT(
  data = MaxDiff, None = 19, id = 1,
  prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff"
)

test_that("Structure of Output", {
  expect_true(base::is.data.frame(recall(data = HOT, id = 1, opts = c(2:9), choice = 10, None = 9)))
})

test_that("Structure of Output", {
  expect_equal(base::nrow(recall(data = HOT, id = 1, opts = c(2:9), choice = 10, None = 9)), 1)
  expect_equal(base::ncol(recall(data = HOT, id = 1, opts = c(2:9), choice = 10, None = 9)), 1)
})

test_that("Labeling correct", {
  expect_equal(base::colnames(recall(data = HOT, id = 1, opts = c(2:9), choice = 10, None = 9)), "recall")
})


test_that("Test plausability of results", {
  expect_true(recall(data = HOT, id = 1, opts = c(2:9), choice = 10, None = 9)[1, 1] <= 100)
})

test_that("Make sure test data is correct", {
  expect_equal(recall(data = HOT, id = 1, opts = c(2:9), choice = 10, None = 9)[1, 1], 91.11)
})

test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- base::as.character(HOT2$choice)
  expect_error(recall(data = HOT2, id = 1, opts = c(2:9), choice = 10, None = 9))
})

test_that("Wrong format Option", {
  names <- base::colnames(HOT)[2:9]

  for (i in 1:base::length(names)){
    expect_true(base::is.numeric(HOT[[names[i]]]))
  }
})

test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(recall(data = HOT2, id = 1, opts = c(2:9), choice = 10, None = 9))
})

test_that("No missings in output", {
  expect_false(base::anyNA(recall(data = HOT, id = 1, opts = c(2:9), choice = 10, None = 9)))
})

test_that("recall() also working with data.frame not created with createHOT()", {
  newHOT <- base::data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T)
  )

  expect_equal(base::nrow(recall(data = newHOT, id = 1, opts = c(2:6), choice = 7, None = 6)), 1)
  expect_equal(base::ncol(recall(data = newHOT, id = 1, opts = c(2:6), choice = 7, None = 6)), 1)

  expect_false(base::anyNA(recall(data = newHOT, id = 1, opts = c(2:6), choice = 7, None = 6)))
})

####################### Test with Grouping variable ########################################

createHOT(data = MaxDiff, None = 19, id = 1,
          prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
          choice = 20, method = "MaxDiff", varskeep = 21)

test_that("Structure of Output", {
  expect_true(base::is.data.frame(recall(data = HOT, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)))
})

test_that("Structure of Output", {
  expect_equal(base::nrow(recall(data = HOT, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)), (base::length(base::unique(HOT$Group)) + 1))
  expect_equal(base::ncol(recall(data = HOT, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)), 2)
})

test_that("Labeling correct", {
  expect_equal(base::colnames(recall(data = HOT, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)), c("Group", "recall"))
})


test_that("Test plausability of results", {

  Results <- recall(data = HOT, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)

  for (i in 1:base::nrow(Results)){
    expect_true(Results[i, 2] <= 100)
  }
})



test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- base::as.character(HOT2$choice)
  expect_error(recall(data = HOT2, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9))
})

test_that("Wrong format Option", {
  names <- base::colnames(HOT)[2:9]

  for (i in 1:base::length(names)){
    expect_true(base::is.numeric(HOT[[names[i]]]))
  }
})


test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(recall(data = HOT2, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9))
})

test_that("No missings in output", {
  expect_false(base::anyNA(recall(data = HOT, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)))
})


test_that("recall() also working with data.frame not created with createHOT()", {
  newHOT <- base::data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T),
    Group = base::sample(c(1,2), 10, replace = T)
  )

  expect_equal(base::nrow(recall(data = newHOT, id = 1, opts = c(2:6), choice = 7, None = 6, Group = 8)), (base::length(base::unique(newHOT$Group)) + 1))
  expect_equal(base::ncol(recall(data = newHOT, id = 1, opts = c(2:6), choice = 7, None = 6, Group = 8)), 2)

  expect_false(base::anyNA(recall(data = newHOT, id = 1, opts = c(2:6), choice = 7, None = 6, Group = 8)))
})


test_that("Right labels of 'Group' variable", {

  # Factor

  HOT2 <- HOT

  ## change 'Group' to factor

  HOT2$Group <- base::factor(HOT2$Group,
                             levels = c(1:3),
                             labels = c("Group 1", "Group 2", "Group 3"))

  lev <- c(base::levels(HOT2$Group))

  Results <- recall(data = HOT2, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])



  # Labelled data

  HOT2 <- HOT

  ## change 'Group' to labelled data
  HOT2$Group <- labelled::labelled(HOT2$Group,
                                   labels = c("Group 1" = 1, "Group 2" = 2, "Group 3" = 3))
  labelled::val_labels(HOT2$Group, prefixed = T)

  lev <- c(base::names(labelled::val_labels(HOT2$Group)))

  Results <- recall(data = HOT2, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])


  # character
  HOT2 <- HOT

  ## change 'Group' to character
  HOT2$Group <- base::as.character(HOT2$Group)

  lev <- c(base::sort(base::unique(HOT2$Group)))

  Results <- recall(data = HOT2, id = 1, Group = 10, opts = c(2:9), choice = 11, None = 9)

  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])

})

