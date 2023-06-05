#' @title  Preparing Holdout task and creating utilities
#'
#' @description Function used to create utilities for validation task.
#'
#' @param data a data frame
#' @param id the column index of \code{id} in \code{data}
#' @param None the column index of \code{None} in \code{data}; if of \code{None} is not included, leave empty
#' @param prod number of options in the Holdout task without the \code{None} option, must be numeric
#' @param prod.levels a list to define the attribute levels of the options (\code{prod})
#' @param method specify the \code{method} your study; needs to be one of the following: MaxDiff, CBC, or ACBC
#' @param interpolate.levels a list of the levels of the variables that should be interpolated. These needs to be the same as provided to Sawtooth Software. Please make sure to provide the whole list. Only needs to be specified for the variables that are coded as 1 (linear) or 2 (piecewise)
#' @param piece.p a list of the column indexes of the lower level and the upper level that should be used for interpolating
#' @param lin.p vector of the column indexes of the linear variables
#' @param coding vector of the coding of each attribute, 0 = part-worth coding, 1 = linear coding, 2 = piecewise coding; please make sure to code linear price of ACBC as piecewise since you have two values to interpolate
#' @param varskeep variables that should be kept in the data frame, use column index
#' @param choice actual choice in the Holdout task
#'
#' @return a data frame
#' @importFrom stats approx
#'
#' @examples
#' library(validateHOT)
#'
#' # MaxDiff example
#' data("MaxDiff")
#' createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   None = 19,
#'   prod = 7,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20
#' )
#'
#' # CBC example
#' data("CBC")
#' createHOT(
#'   data = CBC,
#'   id = 1,
#'   None = 21,
#'   prod = 3,
#'   prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
#'   coding = c(0, 0, 0),
#'   method = "CBC",
#'   choice = 22
#' )
#'
#' # CBC example with linear coding
#' data("CBC_lin")
#' createHOT(
#'   data = CBC_lin,
#'   id = 1,
#'   None = 15,
#'   prod = 3,
#'   prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)),
#'   interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
#'   lin.p = 14,
#'   coding = c(0, 0, 1),
#'   method = "CBC",
#'   varskeep = 17,
#'   choice = 16
#' )
#'
#' # ACBC example with linear price
#' data("ACBC")
#' prod1 <- c(5, 11, 15, 17, 21, 25, 32, 34, 15.99)
#' prod2 <- c(6, 9, 15, 17, 23, 27, 31, 34, 12.99)
#' prod3 <- c(8, 12, 16, 19, 23, 24, 28, 34, 12.99)
#' prod4 <- c(7, 12, 14, 18, 22, 24, 28, 33, 9.99)
#' prod5 <- c(4, 10, 13, 17, 23, 27, 28, 34, 7.99)
#' prod6 <- c(5, 9, 14, 17, 23, 27, 29, 33, 9.99)
#'
#' createHOT(
#'   data = ACBC,
#'   id = 1,
#'   None = 37,
#'   prod = 6,
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   interpolate.levels = list(c(2.093, 27.287)),
#'   piece.p = list(c(35, 36), c(35, 36), c(35, 36), c(35, 36), c(35, 36), c(35, 36)),
#'   coding = c(0, 0, 0, 0, 0, 0, 0, 0, 2),
#'   method = "ACBC",
#'   choice = 38
#' )
#'
#' data("ACBC_interpolate")
#' prod1 <- c(5, 5, 12, 14, 18, 22, 29, 31, 15.99)
#' prod2 <- c(6, 4, 12, 14, 20, 24, 28, 31, 12.99)
#' prod3 <- c(8, 6, 13, 16, 20, 21, 25, 31, 12.99)
#' prod4 <- c(7, 5, 11, 15, 19, 21, 25, 30, 9.99)
#' prod5 <- c(4, 9, 10, 14, 20, 24, 25, 31, 7.99)
#' prod6 <- c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
#'
#' createHOT(
#'   data = ACBC_interpolate,
#'   id = 1,
#'   None = 39,
#'   prod = 6,
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   interpolate.levels = list(c(3, 5, 8, 10), c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)),
#'   piece.p = list(c(36, 37), c(35, 36), c(35, 36), c(33, 34), c(33, 34), c(33, 34)),
#'   lin.p = 9,
#'   coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
#'   method = "ACBC",
#'   choice = 40
#' )
#'
#' @export
createHOT <- function(data, id, None = NULL, prod,
                      prod.levels, interpolate.levels = NULL,
                      piece.p = NULL, lin.p = NULL, coding = NULL,
                      method = c("ACBC" | "CBC" | "MaxDiff"),
                      varskeep = NULL, choice) {
  if (!(base::is.numeric(id)) |
    (!(base::is.numeric(None)) & !(base::is.null(None))) |
    !(base::is.numeric(prod)) |
    (!(base::is.numeric(varskeep)) & !(base::is.null(varskeep)))) {
    stop("Error: Please insert column index. Input needs to be numeric!")
  }

  if (!(base::is.null(coding))) {
    for (i in 1:length(coding)) {
      if (!(base::is.numeric(coding[i]))) {
        stop("Error: Coding only can have numeric input!")
      }
    }
  }

  if (base::missing(method)) {
    stop("Error: method is not defined!")
  }


  if ((method != "ACBC") & (method != "CBC") & (method != "MaxDiff")) {
    stop("Error: Please choose one of the supported methods: MaxDiff, ACBC, CBC!")
  }


  if (method == "MaxDiff" & !(base::is.null(coding))) {
    stop("Error: coding is not not required for ", method, "!")
  }

  if (method == "MaxDiff" & !(base::is.null(interpolate.levels))) {
    stop("Error: interpolate.levels is not not required for ", method, "!")
  }

  if (method == "MaxDiff" & !(base::is.null(piece.p))) {
    stop("Error: piece.p is not not required for ", method, "!")
  }

  if (method == "MaxDiff" & !(base::is.null(lin.p))) {
    stop("Error: lin.p is not not required for ", method, "!")
  }


  if ((method == "ACBC" | method == "CBC") & base::any(coding != 0 & coding != 1 & coding != 2)) {
    stop("Error: Please only use 0 (part-worth), 1 (linear), 2 (piecewise)!")
  }

  if (method == "CBC" & base::any(coding == 2)) {
    stop("Error: Piecewise coding not possible for ", method)
  }

  if (method == "CBC" & !(base::any(coding == 1)) & !(base::is.null(lin.p))) {
    stop("Error: lin.p specified but no 1 in coding!")
  }

  if (method == "ACBC" & !(base::any(coding == 1)) & !(base::is.null(lin.p))) {
    stop("Error: lin.p specified but no 1 in coding!")
  }

  if (method == "ACBC" & !(base::any(coding == 1)) & !(base::is.null(lin.p))) {
    stop("Error: lin.p specified but no 1 in coding!")
  }

  if (method == "ACBC" & !(base::any(coding == 2)) & !(base::is.null(piece.p))) {
    stop("Error: piece.p specified but no 2 in coding!")
  }

  if (!(base::is.list(prod.levels))) {
    stop("Error: prod.levels needs to be a list!")
  }

  if (!(base::is.null(prod.levels))) {
    for (tt in 1:length(prod.levels)) {
      lng <- base::length(prod.levels[[tt]])
      if (lng == 1) {
        var <- prod.levels[[tt]]
        if (!(base::is.numeric(data[[var]]))) {
          stop("Error: Variables included in prod.levels need to be numeric!")
        }
      }

      if (lng > 1) {


        for (lng_lev in 1:lng) {

          if (coding[lng_lev] != 1 & coding[lng_lev] != 2){

            var <- prod.levels[[tt]][lng_lev]
            if (!(base::is.numeric(data[[var]]))) {
              stop("Error: Variables included in prod.levels need to be numeric!")
            }
          }
        }
      }
    }
  }


  if (!(base::is.null(prod.levels))) {
    for (tt in 1:length(prod.levels)) {
      lng <- base::length(prod.levels[[tt]])

      for (lng_lev in 1:lng) {
        if (!(base::is.numeric(prod.levels[[tt]][lng_lev]))) {
          stop("Error: prod.levels needs to be a list with only numeric input!")
        }
      }
    }
  }

  if (!(base::is.list(interpolate.levels)) & !(base::is.null(interpolate.levels))) {
    stop("Error: interpolate.levels needs to be a list!")
  }

  if (!(base::is.null(interpolate.levels))) {
    for (tt in 1:length(interpolate.levels)) {
      lng <- base::length(interpolate.levels[[tt]])

      for (lng_lev in 1:lng) {
        if (!(base::is.numeric(interpolate.levels[[tt]][lng_lev]))) {
          stop("Error: interpolate.levels needs to be a list with only numeric input!")
        }
      }
    }
  }




  if (base::length(prod.levels) != prod) {
    stop("Error: Number of products and defined products do not match!")
  }


  if (!(base::is.null(lin.p)) & !(base::is.vector(lin.p))) {
    stop("Error: lin.p needs to be a vector!")
  }

  if (!(base::is.null(lin.p))) {
    for (ll in 1:base::length(lin.p)) {
      if (!(base::is.numeric(lin.p[ll]))) {
        stop("Error: lin.p needs to be a vector with only numeric values!")
      }

      if (!base::is.numeric(data[[lin.p[ll]]])) {
        stop("Error: Variables included in lin.p needs to be numeric!")
      }
    }
  }

  if (!(base::is.null(lin.p)) & !(base::is.vector(lin.p))) {
    stop("Error: lin.p needs to be a vector")
  }

  if (base::any(coding == 1) & base::is.null(lin.p)) {
    stop("Error: Please specify lin.p!")
  }

  if (base::any(coding == 2) & base::is.null(piece.p)) {
    stop("Error: Please specify piece.p!")
  }

  if (!(base::is.null(piece.p)) & !(base::is.list(piece.p))) {
    stop("Error: piece.p needs to be a list!")
  }

  if (!(base::is.null(piece.p))) {
    for (tt in 1:length(piece.p)) {
      lng <- base::length(piece.p[[tt]])
      if (lng == 1) {
        var <- piece.p[[tt]]
        if (!(base::is.numeric(data[[var]]))) {
          stop("Error: Variables included in piece.p need to be numeric!")
        }
      }

      if (lng > 1) {
        for (lng_lev in 1:lng) {
          var <- piece.p[[tt]][lng_lev]
          if (!(base::is.numeric(data[[var]]))) {
            stop("Error: Variables included in piece.p need to be numeric!")
          }
        }
      }
    }
  }

  if (method == "MaxDiff") {
    coding <- c(base::rep(0, base::length(prod)))
  }

  ######################################################

  Input <- data

  if (!(base::is.null(None))) {
    df <- base::data.frame(base::matrix(nrow = base::nrow(Input), ncol = (prod + 1 + 1)))
  }

  if (base::is.null(None)) {
    df <- base::data.frame(base::matrix(nrow = base::nrow(Input), ncol = (prod + 1)))
  }

  names <- c("ID")

  for (q in 1:prod) {
    Prod <- base::paste0("Option_", q)
    names <- c(names, Prod)
  }

  if (!(base::is.null(None))) {
    names <- c(names, "None")
  }

  colnames(df) <- names

  base::rm(names)

  df[, 1] <- Input[, id]

  df[base::is.na(df)] <- 0




  for (row in 1:base::nrow(df)) {
    for (q in 1:prod) {
      helper <- 1
      linear_pos <- 1
      for (pq in 1:base::length(prod.levels[[q]])) {
        if (coding[pq] == 0) {
          df[row, (q + 1)] <- df[row, (q + 1)] + Input[row, prod.levels[[q]][pq]]
        }
        if (coding[pq] == 1) {
          inter.levels <- interpolate.levels[[helper]]

          # error if xout is larger than maximum
          if (prod.levels[[q]][(pq)] > max(inter.levels)) {
            stop("Error: Extrapolation not possible")
          }

          pos <- lin.p[linear_pos]

          lin.levels_eff <- c(scale(inter.levels, center = T, scale = F))

          lin.low <- lin.levels_eff[1] * Input[row, pos]
          lin.up <- lin.levels_eff[length(lin.levels_eff)] * Input[row, pos]

          util <- base::as.numeric(stats::approx(
            x = c(inter.levels[1], inter.levels[length(inter.levels)]),
            y = c(lin.low, lin.up),
            xout = prod.levels[[q]][(pq)]
          )[2])

          df[row, (q + 1)] <- df[row, (q + 1)] + util

          helper <- helper + 1
          linear_pos <- linear_pos + 1
        }

        if (coding[pq] == 2) {
          inter.levels <- interpolate.levels[[helper]]

          pos.l <- piece.p[[q]][1]
          pos.u <- piece.p[[q]][2]

          interprice <- prod.levels[[q]][(pq)]

          # error if xout is larger than maximum
          if (interprice > max(inter.levels)) {
            stop("Error: Extrapolation not possible")
          }

          lower_b <- max(inter.levels[inter.levels < interprice])
          upper_b <- min(inter.levels[inter.levels >= interprice])

          util <- base::as.numeric(stats::approx(
            x = c(lower_b, upper_b),
            y = c(Input[row, pos.l], Input[row, pos.u]),
            xout = interprice
          )[2])

          df[row, (q + 1)] <- df[row, (q + 1)] + util

          helper <- helper + 1
        }
      }
    }
  }

  if (!(base::is.null(None))) {
    df[, base::ncol(df)] <- Input[, None]
  }

  if (!(base::is.null(varskeep))) {
    add <- Input[, c(id, varskeep)]
    base::colnames(add)[1] <- "ID"
    df <- base::merge(x = df, y = add, by = "ID")
  }

  final_choice <- Input[, c(id, choice)]
  base::colnames(final_choice) <- c("ID", "choice")
  df <- base::merge(x = df, y = final_choice, by = "ID")

  .GlobalEnv$HOT <- df
}
