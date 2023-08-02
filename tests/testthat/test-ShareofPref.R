HOT <- createHOT(
  data = MaxDiff, None = 19,
  id = 1, prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Error if opts is missing", {
  expect_error(shareofpref(data = HOT))
})

test_that("Error if opts has just length 1 ", {
  expect_error(shareofpref(data = HOT, opts = Option_1))
})

test_that("Warning if group contains NA ", {

  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(shareofpref(data = HOT2, opts = c(Option_1:None), group = Group))
})

test_that("Error if alternatives contains NA ", {

  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(shareofpref(data = HOT2, opts = c(Option_1:None), group = Group))
})

test_that("Error if alternatives is not numeric ", {

  HOT2 <- HOT

  HOT2$Option_2 <-base::as.character(HOT2$Option_2)

  expect_error(shareofpref(data = HOT2, opts = c(Option_1:None), group = Group))
})

test_that("Structure of Output data.frame ", {
  expect_true(base::is.data.frame(shareofpref(data = HOT, opts = c(Option_1:None))))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(shareofpref(data = HOT, opts = c(Option_1:None))))
})

test_that("Length of output equals number of alternatives ", {
  expect_equal(base::nrow(shareofpref(data = HOT, opts = c(Option_1:None))), base::length(HOT %>% dplyr::select(c(Option_1:None))))
})

test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(base::nrow(shareofpref(data = HOT, opts = c(Option_1:None), group = Group)), (base::length(base::unique(HOT$Group)) * base::length(HOT %>% dplyr::select(c(Option_1:None)))))
})

test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(base::nrow(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))), (base::length(base::unique(HOT$Group)) * base::length(base::unique(HOT$Group2)) * base::length(HOT %>% dplyr::select(c(Option_1:None)))))
})

test_that("Check output format ", {
  expect_true(base::is.character(shareofpref(data = HOT, opts = c(Option_1:None))[[1]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None))[[2]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None))[[3]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None))[[4]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None))[[5]]))
})

test_that("Check output - 1 group ", {
  expect_true(base::is.character(shareofpref(data = HOT, opts = c(Option_1:None), group = Group)[[2]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = Group)[[3]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = Group)[[4]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = Group)[[5]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = Group)[[6]]))
})

test_that("group output equals group input ", {
  expect_equal(utils::str(shareofpref(data = HOT, opts = c(Option_1:None), group = Group)[[1]]), utils::str(HOT$Group))
})

test_that("group output equals group input - character input ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(utils::str(shareofpref(data = HOT, opts = c(Option_1:None), group = Group2)[[1]]), utils::str(HOT$Group2))
})

test_that("group output equals group input - labelled input ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
                                   labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(shareofpref(data = HOT, opts = c(Option_1:None), group = Group2)[[1]]))
})

test_that("group output equals group input - multiple grouping variables ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
                                   labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(utils::str(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))[[1]]), utils::str(HOT$Group))
  expect_true(labelled::is.labelled(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))[[2]]))
  expect_true(base::is.character(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))[[3]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))[[4]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))[[5]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))[[6]]))
  expect_true(base::is.numeric(shareofpref(data = HOT, opts = c(Option_1:None), group = c(Group, Group2))[[7]]))
})

test_that("shareofpref() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T)
  )
  expect_true(base::is.character(shareofpref(data = newHOT, opts = c(Option_1:Option_5))[[1]]))
  expect_true(base::is.numeric(shareofpref(data = newHOT, opts = c(Option_1:Option_5))[[2]]))
  expect_true(base::is.numeric(shareofpref(data = newHOT, opts = c(Option_1:Option_5))[[3]]))
  expect_true(base::is.numeric(shareofpref(data = newHOT, opts = c(Option_1:Option_5))[[4]]))
  expect_true(base::is.numeric(shareofpref(data = newHOT, opts = c(Option_1:Option_5))[[5]]))
  expect_true(tibble::is_tibble(shareofpref(data = newHOT, opts = c(Option_1:Option_5))))
  expect_false(base::anyNA(shareofpref(data = newHOT, opts = c(Option_1:Option_5))))
  expect_equal(base::sum(shareofpref(data = newHOT, opts = c(Option_1:Option_5))[[2]]), 100)
})

test_that("check whether examples are correct ", {
  expect_equal(shareofpref(data = HOT, opts = c(Option_1:None))[[1]], HOT %>% dplyr::select(c(Option_1:None)) %>% base::colnames())
  expect_equal(base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[2]], 1), c(18.3, 11.3, 4.1, 32.5, 1.9, 10.4, 5.6, 16.0))
  expect_equal(base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[3]], 1), c(4.1, 2.7, 1.5, 4.4, 0.9, 2.7, 1.7, 3.3))
  expect_equal(base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[4]], 1), c(10.2, 6.0, 1.2, 23.8, 0.1, 5.1, 2.1, 9.5))
  expect_equal(base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[5]], 1), c(26.3, 16.6, 7.0, 41.2, 3.7, 15.6, 9.0, 22.4))

  expect_equal(base::sum(shareofpref(data = HOT, opts = c(Option_1:None))[[2]]), 100)
  expect_equal(base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[4]], 1) < base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[5]], 1), rep(TRUE, 8))
  expect_equal(base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[2]], 1) < base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[5]], 1), rep(TRUE, 8))
  expect_equal(base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[2]], 1) > base::round(shareofpref(data = HOT, opts = c(Option_1:None))[[4]], 1), rep(TRUE, 8))
})
