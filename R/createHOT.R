#' @title  Preparing Holdout task and creating utilities
#'
#' @description Function used to create utilities for validation task.
#'
#' @param data a data frame
#' @param id the column index of \code{id} in \code{data}
#' @param None the column index of \code{None} in \code{data}; if of \code{None} is not included, leave empty
#' @param prod number of options in the Holdout task without the \code{None} option, must be numeric
#' @param prod.levels define the attribute levels of the products, must be a list
#' @param method specify the \code{method} your study; needs to be one of the following: MaxDiff, CBC, or ACBC
#' @param interpolate.levels xyz
#' @param piece.p xyz
#' @param lin.p xyz
#' @param coding xyz
#' @param varskeep variables that should be kept in the data frame, use column index
#' @param choice actual choice in the Holdout task
#'
#' @return a data frame
#' @importFrom stats approx
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(
#'   data = MaxDiff, None = 19,
#'   id = 1, prod = 7,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   choice = 20, method = "MaxDiff"
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

  if (!(base::is.list(prod.levels))) {
    stop("Error: prod.levels needs to be a list!")
  }

  if (!(base::is.null(prod.levels))) {
    for (tt in 1:length(prod.levels)) {
      lng <- base::length(prod.levels[[tt]])

      for (lng_lev in 1:length(lng)) {
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

      for (lng_lev in 1:length(lng)) {
        if (!(base::is.numeric(interpolate.levels[[tt]][lng_lev]))) {
          stop("Error: interpolate.levels needs to be a list with only numeric input!")
        }
      }
    }
  }

  if ((method != "ACBC") & (method != "CBC") & (method != "MaxDiff")) {
    stop("Error: Please choose one of the supported methods: MaxDiff, ACBC, CBC")
  }

  if (base::length(prod.levels) != prod) {
    stop("Error: Number of products and defined products do not match!")
  }


  if (method == "ACBC" | method == "CBC") {
    for (ll in 1:base::length(interpolate.levels)) {
      if (base::length(coding) != base::length(interpolate.levels[[ll]])) {
        stop("Error: length of coding and length of one of the alternatives is not equal!")
      }
    }
  }

  if ((method == "ACBC" | method == "CBC") & base::any(coding != 0 & coding != 1 & coding != 2)) {
    stop("Error: Please only use 0 (part-worth), 1 (linear), 2 (piecewise)!")
  }

  if (!(base::is.null(lin.p)) & !(base::is.vector(lin.p))) {
    stop("Error: lin.p needs to be a vector")
  }

  if (method == "MaxDiff") {
    coding <- c(base::rep(0, base::length(prod)))
  }

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
