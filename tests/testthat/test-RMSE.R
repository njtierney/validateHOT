library(ValiDatHOT)
data(MaxDiff)

####################### Test wo Grouping variable ########################################

createHOT(
  data = MaxDiff, None = 19, id = 1,
  prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff"
)

test_that("Structure of Output", {
  expect_true(is.data.frame(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 10)))
})

test_that("Structure of Output", {
  expect_equal(nrow(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 10)), 1)
  expect_equal(ncol(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 10)), 1)
})

test_that("Labeling correct", {
  expect_equal(colnames(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 10)), "RMSE")
})

test_that("Count of correct predicted people", {
  expect_equal(base::round(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 10)[1, 1], digits = 3), 7.882)
})

test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- as.character(HOT2$choice)
  expect_error(RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 10))
})

test_that("Wrong format Option", {
  HOT2 <- HOT
  HOT2$Option_2 <- as.character(HOT2$Option_2)
  expect_error(RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 10))
})


test_that("Test plausability of results", {

  expect_true(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 10)[1, 1] >= 0)

})

test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 10))
})

test_that("No missings in output", {
  expect_false(base::anyNA(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 10)))
})


test_that("RMSE() also working with data.frame not created with createHOT()", {
  newHOT <- data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:4), 10, replace = T)
  )

  expect_equal(nrow(RMSE(data = newHOT, id = 1, opts = c(2:5), choice = 6)), 1)
  expect_equal(ncol(RMSE(data = newHOT, id = 1, opts = c(2:5), choice = 6)), 1)


  expect_false(base::anyNA(RMSE(data = newHOT, id = 1, opts = c(2:5), choice = 6)))
})


####################### Test with Grouping variable ########################################

createHOT(data = MaxDiff, None = 19, id = 1,
          prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
          choice = 20, method = "MaxDiff", varskeep = 21)

test_that("Structure of Output", {
  expect_true(is.data.frame(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)))
})

test_that("Structure of Output", {
  expect_equal(nrow(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), (length(unique(HOT$Group)) + 1))
  expect_equal(ncol(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), 2)
})

test_that("Labeling correct", {
  expect_equal(colnames(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), c("Group", "RMSE"))
})

test_that("Test plausability of results", {

  Results <- RMSE(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)

  for (i in 1:nrow(Results)){
    expect_true(Results[i, 2] >= 0)
  }
})



test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- as.character(HOT2$choice)
  expect_error(RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})

test_that("Wrong format Option", {
  HOT2 <- HOT
  HOT2$Option_2 <- as.character(HOT2$Option_2)
  expect_error(RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})


test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})

test_that("No missings in output", {
  expect_false(base::anyNA(RMSE(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)))
})


test_that("MedAE() also working with data.frame not created with createHOT()", {
  newHOT <- data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:4), 10, replace = T),
    Group = base::sample(c(1,2), 10, replace = T)
  )

  expect_equal(nrow(RMSE(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)), (length(unique(newHOT$Group)) + 1))
  expect_equal(ncol(RMSE(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)), 2)

  expect_false(base::anyNA(RMSE(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)))
})


test_that("Right labels of 'Group' variable", {

  # Factor

  HOT2 <- HOT

  ## change 'Group' to factor

  HOT2$Group <- base::factor(HOT2$Group,
                             levels = c(1:3),
                             labels = c("Group 1", "Group 2", "Group 3"))

  lev <- c(base::levels(HOT2$Group))

  Results <- RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


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

  Results <- RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])


  # character
  HOT2 <- HOT

  ## change 'Group' to character
  HOT2$Group <- as.character(HOT2$Group)

  lev <- c(base::sort(base::unique(HOT2$Group)))

  Results <- RMSE(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])


})


