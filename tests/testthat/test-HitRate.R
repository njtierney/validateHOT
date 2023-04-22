library(ValiDatHOT)
data(MaxDiff)

####################### Test wo Grouping variable ########################################

createHOT(
  data = MaxDiff, None = 19, id = 1,
  prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff"
)
HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)

test_that("Structure of Output", {
  expect_true(is.data.frame(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)))
})

test_that("Structure of Output", {
  expect_equal(nrow(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)), 3)
  expect_equal(ncol(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)), 1)
})

test_that("Labeling correct", {
  expect_equal(row.names(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)), c("chance", "no.", "%"))
  expect_equal(colnames(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)), "HitRate")
})



test_that("Chance Level correct", {
  expect_equal(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[1, 1], (1 / 8 * 100))
})

test_that("Count of correct predicted people", {
  expect_equal(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[2, 1], 39)
})

test_that("Test plausability of results", {
  expect_true(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[1, 1] <= 100 & HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[1, 1] >= 0)
  expect_true(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[2, 1] <= nrow(HOT))
  expect_true(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[3, 1] <= 100 & HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[3, 1] >= 0)
})

test_that("Hit Rate % correct", {
  expect_equal(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[3, 1], (HitRate(data = HOT, id = 1, opts = c(2:9), choice = 10)[2, 1] / nrow(HOT) * 100))
})

test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- as.character(HOT2$choice)
  expect_error(HitRate(data = HO2, id = 1, opts = c(2:9), choice = 10))
})

test_that("Wrong format Option", {
  HOT2 <- HOT
  HOT2$Option_2 <- as.character(HOT2$Option_2)
  expect_error(HitRate(data = HO2, id = 1, opts = c(2:9), choice = 10))
})


test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(HitRate(data = HO2, id = 1, opts = c(2:9), choice = 10))
})


test_that("HitRate() also working with data.frame not created with createHOT()", {
  newHOT <- data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:4), 10, replace = T)
  )

  expect_equal(nrow(HitRate(data = newHOT, id = 1, opts = c(2:5), choice = 6)), 3)
  expect_equal(ncol(HitRate(data = newHOT, id = 1, opts = c(2:5), choice = 6)), 1)
  expect_equal(HitRate(data = newHOT, id = 1, opts = c(2:5), choice = 6)[1, 1], (1 / (length(newHOT) - 2) * 100))
})






####################### Test with Grouping variable ########################################

createHOT(data = MaxDiff, None = 19, id = 1,
           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
           choice = 20, method = "MaxDiff", varskeep = 21)

test_that("Structure of Output", {
  expect_true(is.data.frame(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)))
})

test_that("Structure of Output", {
  expect_equal(nrow(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), (length(unique(HOT$Group)) + 1))
  expect_equal(ncol(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), 4)
})

test_that("Labeling correct", {
  expect_equal(colnames(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)), c("Group", "no.", "perc.", "chance"))
})


test_that("Chance Level correct", {
  expect_equal(mean(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)$chance), (1 / 8 * 100))
})

test_that("Count of correct predicted people", {
  expect_equal(HitRate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)[1, 2], 39)
})

test_that("Test plausability of results", {

  Results <- HitRate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)

  for (i in 1:nrow(Results)){
    expect_true(Results[i, 2] <= nrow(HOT))

    expect_true(Results[i, 3] <= 100)

    expect_true(Results[i, 4] <= 100)
  }
})



test_that("Wrong format Choice", {
  HOT2 <- HOT
  HOT2$choice <- as.character(HOT2$choice)
  expect_error(HitRate(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})

test_that("Wrong format Option", {
  HOT2 <- HOT
  HOT2$Option_2 <- as.character(HOT2$Option_2)
  expect_error(HitRate(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})


test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(HitRate(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})


test_that("HitRate() also working with data.frame not created with createHOT()", {
  newHOT <- data.frame(
    ID = c(1:10),
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:4), 10, replace = T),
    Group = base::sample(c(1,2), 10, replace = T)
  )

  expect_equal(nrow(HitRate(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)), (length(unique(newHOT$Group)) + 1))
  expect_equal(ncol(HitRate(data = newHOT, id = 1, opts = c(2:5), choice = 6, Group = 7)), 4)
})


test_that("Right labels of 'Group' variable", {

  # Factor

  HOT2 <- HOT

  ## change 'Group' to factor

  HOT2$Group <- base::factor(HOT2$Group,
                             levels = c(1:3),
                             labels = c("Group 1", "Group 2", "Group 3"))

  lev <- c(base::levels(HOT2$Group))

  Results <- HitRate(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


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

  Results <- HitRate(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])


  # character
  HOT2 <- HOT

  ## change 'Group' to character
  HOT2$Group <- as.character(HOT2$Group)

  lev <- c(base::sort(base::unique(HOT2$Group)))

  Results <- HitRate(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10)


  expect_true(Results$Group[1] == "All")
  expect_true(Results$Group[2] == lev[1])
  expect_true(Results$Group[3] == lev[2])
  expect_true(Results$Group[4] == lev[3])


})

test_that("Missings", {
  HOT2 <- HOT
  HOT2[1, 5] <- NA
  expect_error(HitRate(data = HOT2, id = 1, opts = c(2:9), choice = 11, Group = 10))
})

