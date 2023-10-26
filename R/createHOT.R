#' @title  Preparing holdout/validation task and creating utilities
#'
#' @description Function used to create utilities for validation task.
#' Currently not working with alternative-specific designs.
#'
#' @param data A data frame with all relevant variables.
#' @param id A vector of column index of unique identifier in \code{data}.
#' @param none An optional column index to specify \code{none}
#' alternative in \code{data}.
#' @param prod A vector that specifies the number of alternatives in the
#' holdout/validation task (does not count the \code{none} alternative). The
#' input of \code{prod} has to be numeric.
#' @param prod.levels A list to define the attribute levels of the
#' alternatives (\code{prod}).
#' @param interpolate.levels A list of the levels of the variables that should
#' be interpolated. These have to be the same as provided to Sawtooth Software.
#' Please make sure to provide the whole list. Only has to be specified for the
#' variables that are coded as '1' (linear) or '2' (piecewise).
#' @param piece.p A list of the column indexes of the lower level and the upper
#' level that should be used for interpolating.
#' @param lin.p A vector of the column indexes of the linear variables.
#' @param coding A vector of the coding of each attribute, '0' = part-worth
#' coding,'1' = linear coding, '2' = piecewise coding; please make sure to code
#' linear price of ACBC as piecewise since you have two values to interpolate.
#' @param method A character to specify the \code{method} of your study.
#' \code{method} has to be one of the following three: "MaxDiff", "CBC", or "ACBC".
#' @param varskeep A vector of column index(es) of variables that should be kept
#' in the data frame.
#' @param choice Actual choice in the holdout/validation task.
#'
#' @details
#' In order to test validation metrics of a holdout/validation task, the
#' holdout/validation task first has to be created.
#' This is done by the function \code{createHOT}.
#' Make sure to upload the raw utilities of your study.
#' Afterwards, the function will create the utilities based on the additive
#' utility model (Rao, 2014, p. 82).
#'
#' \code{data} has to be a data frame with **raw** scores of the attribute
#' levels.
#'
#' \code{id} has to be the column index of the id (unique for each participant)
#' in data frame.
#'
#' \code{none} has to be specified in case a \code{none} alternative is
#' included in holdout/validation task, please specify this by specifying the
#' column index of \code{none} alternative, otherwise leave it empty.
#'
#' \code{prod} should specify the total number of alternatives included in your
#' holdout/validation task (excluding the \code{none} alternative).
#'
#' \code{prod.levels} specifies the attribute levels for each alternative.
#' Input for \code{prod.levels} has to be a list. In case
#' \code{method = "MaxDiff"} this will only be a list of the column indexes of
#' the alternatives in the holdout/validation task. If \code{method = "CBC"} or
#' \code{method = "ACBC"} use a vector to specify the attribute levels for each
#' alternative. For \code{method = "CBC"} and \code{method = "ACBC"}
#' also the column index(es) of the attribute level has to be specified.
#' In case values for one attribute are interpolated (assuming linear or
#' piecewise coding), the value to be interpolated has to be specified.
#' In addition, \code{lin.p} and/or \code{piece.p}, \code{interpolate.levels}
#' as well as \code{coding} have to be specified.
#'
#' \code{interpolate.levels} has to be specified in case interpolating is used
#' (only if variables are coded as linear or piecewise).
#' If scaled or centered values were used for hierarchical bayes (HB)
#' estimation, these have to be specified in this case.
#' All values have to be specified. For example, if one linear coded attribute
#' had 5 levels, all 5 levels have to be inserted. In case for linear coded
#' price for \code{method = "ACBC"}, specify both lower bound and upper
#' bound and code as piecewise in \code{coding}.
#' For piecewise coded price, specify each breakpoint.
#' Input for \code{interpolate.levels} has to be a list.
#'
#' \code{piece.p} has to be specified in case a variable is coded as
#' piecewise (see coding). Positions of both lower and upper bound have to
#' be specified. In case interpolated values (see
#' prod.levels) is equal to a lower or upper bound, this can be specified
#' either as lower or upper bound. Input for \code{piece.p} has to be a list.
#'
#' \code{lin.p} has to be specified in case a variable is coded as linear
#' (see coding). Since for linear coding (except for price
#' in \code{method = "ACBC"}) only one coefficient is provided in the output,
#' just this column index has to be specified, consequently, input
#' for \code{piece.p} has to be a vector.
#'
#' \code{coding} has to be specified for if \code{method = "CBC"}
#' or \code{method = "ACBC"}. \code{0} has to be used for parth-worth
#' coding, \code{1} for linear coding, and \code{2} for piecewise coding.
#' In case \code{method = "ACBC"} and linear price function is used, this
#' variable has to be coded as piecewise (\code{2}) in this case. In
#' case \code{method} is set to \code{"MaxDiff"}, leave
#' \code{coding} empty. Input for \code{coding} has to be a vector.
#'
#' \code{method} specifies the preference measurement method. Can be set to
#' \code{"MaxDiff"}, \code{"CBC"}, or \code{"ACBC"}.
#'
#' \code{varskeep} has to be specified in case other variables should be kept
#' in the data frame (for example, a grouping variable). Input
#' for \code{varskeep} has to be a vector with the column index(es) of the
#' variable(s) that should be kept.
#'
#' \code{choice} specifies the column index of the acutal choice in the
#' holdout/validation task.
#'
#' @return a data frame
#' @importFrom stats approx
#'
#' @references {
#'
#' Rao, V. R. (2014). \emph{Applied Conjoint Analysis}. Heidelberg: Springer
#' Berlin. \verb{https://doi.org/10.1007/978-3-540-87753-0}
#'
#' }
#'
#' @examples
#' \dontrun{
#' # MaxDiff example
#' HOT_MD <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   none = 19,
#'   prod = 7,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20
#' )
#'
#' # CBC example
#' HOT_CBC <- createHOT(
#'   data = CBC,
#'   id = 1,
#'   none = 21,
#'   prod = 3,
#'   prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
#'   coding = c(0, 0, 0),
#'   method = "CBC",
#'   choice = 22
#' )
#'
#' # CBC example with linear coding
#' HOT_CBC_lin <- createHOT(
#'   data = CBC_lin,
#'   id = 1,
#'   none = 15,
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
#' prod1 <- c(5, 11, 15, 17, 21, 25, 32, 34, 15.99)
#' prod2 <- c(6, 9, 15, 17, 23, 27, 31, 34, 12.99)
#' prod3 <- c(8, 12, 16, 19, 23, 24, 28, 34, 12.99)
#' prod4 <- c(7, 12, 14, 18, 22, 24, 28, 33, 9.99)
#' prod5 <- c(4, 10, 13, 17, 23, 27, 28, 34, 7.99)
#' prod6 <- c(5, 9, 14, 17, 23, 27, 29, 33, 9.99)
#'
#' HOT_ACBC <- createHOT(
#'   data = ACBC,
#'   id = 1,
#'   none = 37,
#'   prod = 6,
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   interpolate.levels = list(c(2.093, 27.287)),
#'   piece.p = list(
#'     c(35, 36), c(35, 36), c(35, 36),
#'     c(35, 36), c(35, 36), c(35, 36)
#'   ),
#'   coding = c(0, 0, 0, 0, 0, 0, 0, 0, 2),
#'   method = "ACBC",
#'   choice = 38
#' )
#'
#' prod1 <- c(5, 5, 12, 14, 18, 22, 29, 31, 15.99)
#' prod2 <- c(6, 4, 12, 14, 20, 24, 28, 31, 12.99)
#' prod3 <- c(8, 6, 13, 16, 20, 21, 25, 31, 12.99)
#' prod4 <- c(7, 5, 11, 15, 19, 21, 25, 30, 9.99)
#' prod5 <- c(4, 9, 10, 14, 20, 24, 25, 31, 7.99)
#' prod6 <- c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
#'
#' HOT_ACBC_inter <- createHOT(
#'   data = ACBC_interpolate,
#'   id = 1,
#'   none = 39,
#'   prod = 6,
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   interpolate.levels = list(
#'     c(3, 5, 8, 10),
#'     c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
#'   ),
#'   piece.p = list(
#'     c(36, 37), c(35, 36), c(35, 36),
#'     c(33, 34), c(33, 34), c(33, 34)
#'   ),
#'   lin.p = 9,
#'   coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
#'   method = "ACBC",
#'   choice = 40
#' )
#' }
#' @export
createHOT <- function(data, id, none = NULL, prod,
                      prod.levels, interpolate.levels = NULL,
                      piece.p = NULL, lin.p = NULL, coding = NULL,
                      method = c("ACBC", "CBC", "MaxDiff"),
                      varskeep = NULL, choice) {
  # test whether input is numeric
  if (!(base::is.numeric(id)) |
    (!(base::is.numeric(none)) & !(base::is.null(none))) |
    !(base::is.numeric(prod)) |
    (!(base::is.numeric(varskeep)) & !(base::is.null(varskeep)))) {
    base::stop("Error: Please insert column index (numeric inpute)!")
  }

  # test numeric input for coding
  if (!(base::is.null(coding))) {
    for (i in 1:base::length(coding)) {
      if (!(base::is.numeric(coding[i]))) {
        base::stop("Error: 'coding' only can have numeric input!")
      }
    }
  }

  # test whether method is specified
  if (base::missing(method)) {
    base::stop("Error: 'method' is not defined!")
  }

  # test whether method is correctly specified
  if ((method != "ACBC") & (method != "CBC") & (method != "MaxDiff")) {
    base::stop(
      "Error: Please choose one of the supported methods: 'MaxDiff',",
      " 'ACBC', 'CBC'!"
    )
  }

  # test whether coding is empty if method == MaxDiff
  if (method == "MaxDiff" & !(base::is.null(coding))) {
    base::stop("Error: 'coding' is not required for ", method, "!")
  }

  # test whether interpolate levels is empty is specified if method == MaxDiff
  if (method == "MaxDiff" & !(base::is.null(interpolate.levels))) {
    base::stop("Error: 'interpolate.levels' is not required for ", method, "!")
  }

  # test whether piece.p is empty is specified if method == MaxDiff
  if (method == "MaxDiff" & !(base::is.null(piece.p))) {
    base::stop("Error: 'piece.p' is not required for ", method, "!")
  }

  # test whether lin.p is empty is specified if method == MaxDiff
  if (method == "MaxDiff" & !(base::is.null(lin.p))) {
    base::stop("Error: 'lin.p' is not required for ", method, "!")
  }

  # test whether coding only includes 0, 1, 2
  if ((method == "ACBC" | method == "CBC") &
    base::any(coding != 0 & coding != 1 & coding != 2)) {
    base::stop(
      "Error: Please only use '0' (for part-worth), '1' (for linear)",
      ", or '2' (for piecewise)!"
    )
  }

  # test whether CBC is specified and no coding equal to 2
  if (method == "CBC" & base::any(coding == 2)) {
    base::stop("Error: Piecewise coding not possible for ", method, "!")
  }

  # test whether CBC is used, one variable linear coded however
  # position not specified (or other way around)
  if (method == "CBC" & !(base::any(coding == 1)) & !(base::is.null(lin.p))) {
    base::stop("Error: 'lin.p' specified but no '1' in coding!")
  }

  # test whether ACBC is used, one variable linear coded however
  # position not specified  (or other way around)
  if (method == "ACBC" & !(base::any(coding == 1)) & !(base::is.null(lin.p))) {
    base::stop("Error: 'lin.p' specified but no '1' in coding!")
  }

  # test whether ACBC is used, one variable piecewise coded however
  # position not specified  (or other way around)
  if (method == "ACBC" & !(base::any(coding == 2)) &
    !(base::is.null(piece.p))) {
    base::stop("Error: 'piece.p' specified but no '2' in coding!")
  }

  # test input of prod.levels
  if (!(base::is.list(prod.levels))) {
    base::stop("Error: Input of 'prod.levels' has to be a list!")
  }

  # test input of list in prod.levels
  if (!(base::is.null(prod.levels))) {
    for (tt in 1:base::length(prod.levels)) {
      lng <- base::length(prod.levels[[tt]])

      for (lng_lev in 1:lng) {
        if (!(base::is.numeric(prod.levels[[tt]][lng_lev]))) {
          base::stop(
            "Error: Input of 'prod.levels' has to be a list ",
            "with only numeric values!"
          )
        }
      }
    }
  }

  # test variables of prod.levels
  if (!(base::is.null(prod.levels))) {
    for (tt in 1:base::length(prod.levels)) {
      lng <- base::length(prod.levels[[tt]])
      if (lng == 1) {
        var <- prod.levels[[tt]]
        if (!(base::is.numeric(data[[var]]))) {
          base::stop(
            "Error: Variables included in 'prod.levels' ",
            "have to be numeric!"
          )
        }
      }

      if (lng > 1) {
        for (lng_lev in 1:lng) {
          if (coding[lng_lev] != 1 & coding[lng_lev] != 2) {
            var <- prod.levels[[tt]][lng_lev]
            if (!(base::is.numeric(data[[var]]))) {
              base::stop(
                "Error: Variables included in 'prod.levels' ",
                "have to be numeric!"
              )
            }
          }
        }
      }
    }
  }

  # test input of interpolate levels
  if (!(base::is.list(interpolate.levels)) &
    !(base::is.null(interpolate.levels))) {
    base::stop("Error: Input of 'interpolate.levels' has to be a list!")
  }

  # test variables of interpolate.levels
  if (!(base::is.null(interpolate.levels))) {
    for (tt in 1:base::length(interpolate.levels)) {
      lng <- base::length(interpolate.levels[[tt]])

      for (lng_lev in 1:lng) {
        if (!(base::is.numeric(interpolate.levels[[tt]][lng_lev]))) {
          base::stop(
            "Error: Input of 'interpolate.levels' has to be a list ",
            "with only numeric values!"
          )
        }
      }
    }
  }

  # test whether input is in accordance
  if (base::length(prod.levels) != prod) {
    base::stop("Error: Number of products and defined products do not match!")
  }

  # test lin.p variables format
  if (!(base::is.null(lin.p))) {
    for (ll in 1:base::length(lin.p)) {
      if (!(base::is.numeric(lin.p[ll]))) {
        base::stop(
          "Error: Input of 'lin.p' has to be a vector ",
          "with only numeric values!"
        )
      }

      if (!base::is.numeric(data[[lin.p[ll]]])) {
        base::stop("Error: Variables included in 'lin.p' have to be numeric!")
      }
    }
  }

  # test whether coding indicated linear coded variable, however,
  # not specified
  if (base::any(coding == 1) & base::is.null(lin.p)) {
    base::stop("Error: Please specify 'lin.p'!")
  }

  # test whether coding indicated piecewise coded variable, however,
  # not specified
  if (base::any(coding == 2) & base::is.null(piece.p)) {
    base::stop("Error: Please specify 'piece.p'!")
  }

  # test input of piece.p
  if (!(base::is.null(piece.p)) & !(base::is.list(piece.p))) {
    base::stop("Error: Input of 'piece.p' has to be a list!")
  }

  # test variables specified in piece.p
  if (!(base::is.null(piece.p))) {
    for (tt in 1:base::length(piece.p)) {
      lng <- base::length(piece.p[[tt]])
      if (lng == 1) {
        var <- piece.p[[tt]]
        if (!(base::is.numeric(data[[var]]))) {
          base::stop(
            "Error: Variables included in 'piece.p' ",
            "have to be numeric!"
          )
        }
      }

      if (lng > 1) {
        for (lng_lev in 1:lng) {
          var <- piece.p[[tt]][lng_lev]
          if (!(base::is.numeric(data[[var]]))) {
            base::stop(
              "Error: Variables included in 'piece.p' ",
              "have to be numeric!"
            )
          }
        }
      }
    }
  }

  ######################################################

  # if MaxDiff is used, set coding for each alternative to 0
  if (method == "MaxDiff") {
    coding <- c(base::rep(0, base::length(prod)))
  }

  ######################################################

  # store the input
  Input <- data

  # create empty data frame to store - none alternative specified
  if (!(base::is.null(none))) {
    df <- base::data.frame(base::matrix(
      nrow = base::nrow(Input),
      ncol = (prod + 1 + 1)
    ))
  }

  # create empty data frame to store - no none alternative specified
  if (base::is.null(none)) {
    df <- base::data.frame(base::matrix(
      nrow = base::nrow(Input),
      ncol = (prod + 1)
    ))
  }

  # prepare output names
  names <- c("ID")

  for (q in 1:prod) {
    Prod <- base::paste0("Option_", q)
    names <- c(names, Prod)
  }

  if (!(base::is.null(none))) {
    names <- c(names, "None")
  }

  # assign column names to new created data frame 'df'
  base::colnames(df) <- names

  # delete names
  base::rm(names)

  # store the id in the new data frame
  df[, 1] <- Input[, id]

  # overwrite all NAs by 0
  df[base::is.na(df)] <- 0


  for (row in 1:base::nrow(df)) { # repeat procedure for each row
    for (q in 1:prod) { # and for each prod
      helper <- 1 # define helper variables
      linear_pos <- 1 # define helper variables

      # loop for each level specified for an alternative (prod)
      for (pq in 1:base::length(prod.levels[[q]])) {
        if (coding[pq] == 0) { # only run for part-worth coded variable

          # in case of part-worth coding the utilitiy can be added
          df[row, (q + 1)] <- df[row, (q + 1)] +
            Input[row, prod.levels[[q]][pq]]
        }
        if (coding[pq] == 1) { # loop for each linear coded attribute

          # extract the interpolate levels
          inter.levels <- interpolate.levels[[helper]]

          # error if xout is larger than maximum
          if (prod.levels[[q]][(pq)] > base::max(inter.levels)) {
            base::stop("Error: Extrapolation not possible!")
          }

          pos <- lin.p[linear_pos] # extract the column index of the variable

          # center the attribute levels
          lin.levels_eff <- c(base::scale(inter.levels,
            center = TRUE, scale = FALSE
          ))

          # get utility of lower bound of linear coded attribute
          lin.low <- lin.levels_eff[1] * Input[row, pos]

          # get utility of upper bound of linear coded attribute
          lin.up <- lin.levels_eff[base::length(lin.levels_eff)] *
            Input[row, pos]

          # finally extrapolate utility for specified value
          util <- base::as.numeric(stats::approx(
            x = c(inter.levels[1], inter.levels[base::length(inter.levels)]),
            y = c(lin.low, lin.up),
            xout = prod.levels[[q]][(pq)]
          )[2])

          # finally store the utility
          df[row, (q + 1)] <- df[row, (q + 1)] + util

          # and set both helper variables one up
          helper <- helper + 1
          linear_pos <- linear_pos + 1
        }

        if (coding[pq] == 2) { # loop for piecewise coded variable

          # extract the interpolate levels
          inter.levels <- interpolate.levels[[helper]]

          pos.l <- piece.p[[q]][1] # store position of lower bound
          pos.u <- piece.p[[q]][2] # store position of upper bound

          # extract price that should be interpolated
          interprice <- prod.levels[[q]][(pq)]

          # error if xout is larger than maximum
          if (interprice > base::max(inter.levels)) {
            base::stop("Error: Extrapolation not possible!")
          }

          # extract the breakpoint below the price to be interpolated
          lower_b <- base::max(inter.levels[inter.levels < interprice])

          # extract the breakpoint equal or above the price to be interpolated
          upper_b <- base::min(inter.levels[inter.levels >= interprice])

          # interpolate
          util <- base::as.numeric(stats::approx(
            x = c(lower_b, upper_b),
            y = c(Input[row, pos.l], Input[row, pos.u]),
            xout = interprice
          )[2])

          df[row, (q + 1)] <- df[row, (q + 1)] + util # add to utility

          helper <- helper + 1 # add 1 to helper variables
        }
      }
    }
  }

  # finally add none utility
  if (!(base::is.null(none))) {
    df[, base::ncol(df)] <- Input[, none]
  }

  # in case varskeep specified
  if (!(base::is.null(varskeep))) {
    add <- Input[, c(id, varskeep)] # store variables
    base::colnames(add)[1] <- "ID" # rename ID for merging purposes
    df <- base::merge(x = df, y = add, by = "ID") # merge
  }

  # store the final choice
  final_choice <- Input[, c(id, choice)]
  # rename variables for merging purposes
  base::colnames(final_choice) <- c("ID", "choice")
  df <- base::merge(x = df, y = final_choice, by = "ID") # merge

  # store the finished data frame in new object
  HOT <- df

  # return the newly created data frame
  return(HOT)
}
