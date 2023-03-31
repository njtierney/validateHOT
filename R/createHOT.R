#' @title  Preparing Holdout task and creating utilities
#'
#' @description Function used to create utilities for a specific Holdout task.
#'
#' @param data a data frame
#' @param id the column index of \code{id} in \code{data}
#' @param None the column index of \code{None} in \code{data}; if of \code{None} is not included, leave empty
#' @param prod number of options in the Holdout task without the \code{None} option, must be numeric
#' @param x define the attribute levels of the products
#' @param method specify the \code{method} your study; needs to be one of the following: MaxDiff, CBC, or ACBC
#' @param price whether you use \code{"fixed"} price level or \code{"interpolate"} if you use \code{"method = 'ACBC'"}, if you use \code{"method = 'CBC'"} please set \code{"price"}
#' either to \code{"linear"} or \code{"piecewise"}
#' @param price_low lower price border
#' @param price_high upper price border
#' @param price_low_po column index of lower price
#' @param price_high_po column index of upper price
#' @param varskeep variables that should be kept in the data frame, use column index
#' @param choice actual choice in the Holdout task
#'
#' @return a data frame
#' @importFrom stats approx
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff")
#'
#'
#'
#' @export
createHOT <- function(data, id, None = NULL, prod, x, method = c("ACBC" | "CBC" | "MaxDiff"), price = NULL, price_low = NULL, price_high = NULL, price_low_po = NULL,
                       price_high_po = NULL, varskeep = NULL, choice) {
  if (!(base::is.numeric(id)) | (!(base::is.numeric(None)) & !(base::is.null(None))) | !(base::is.numeric(prod)) |
    (!(base::is.numeric(price_low)) & !(base::is.null(price_low))) |
    (!(base::is.numeric(price_high)) & !(base::is.null(price_high))) | (!(base::is.numeric(price_low_po)) & !(base::is.null(price_low_po))) |
    (!(base::is.numeric(price_high_po)) & !(base::is.null(price_high_po))) | (!(base::is.numeric(varskeep)) & !(base::is.null(varskeep)))) {
    stop("Error: Please insert column index. Needs to be numeric!")
  }

  if ((method != "ACBC") & (method != "CBC") & (method != "MaxDiff")){
    stop("Error: Please choose one of the supported methods: MaxDiff, ACBC, CBC")
  }


  if (method == "ACBC") {
    if ((price == "linear" | price == "piecewise") & (base::is.null(price_high) | base::is.null(price_high_po) | base::is.null(price_low) | base::is.null(price_low_po))) {
      stop("Error: Some variables are not defined!")
    }
    if (price == "linear" & (base::length(price_high) > 1 | base::length(price_high_po) > 1 | base::length(price_low_po) > 1 | base::length(price_low_po) > 1)) {
      stop("Error: Too many variables defined for price!")
    }
    if (price == "piecewise" & (base::length(price_high) != prod | base::length(price_high_po) != prod | base::length(price_low_po) != prod | base::length(price_low_po) != prod)) {
      stop("Error: Variables defined do not match number of products!")
    }
  }

  if (method == "CBC" & base::is.null(price)) {
    stop("Error: Please specify whether price is fixed parameter or should be interpolated!")
  }

  if (method == "CBC" & !(base::is.null(price))) {
    if (price != "fixed" & price != "interpolate") {
      stop("Error: Please specify whether price is fixed parameter or should be interpolated!")
    }
  }

  if (method == "MaxDiff") {
    if (!(base::is.null(price_low)) | !(base::is.null(price_high)) | !(base::is.null(price_low_po)) | !(base::is.null(price_high_po))) {
      stop("Error: Does not need to specify those for ", method, "!")
    }
  }

  if (method == "MaxDiff") {
    if (!(base::is.null(price))) {
      stop("Error: Does not need to specify price type for ", method, "!")
    }
  }



  if (base::length(x) != prod) {
    stop("Error: Number of products and defined products do not match!")
  }

  if (method == "CBC") {
    if (is.null(price)) {
      c <- data

      if (!(base::is.null(None))) {
        df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1 + 1)))
      }

      if (base::is.null(None)) {
        df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1)))
      }

      names <- c("ID")

      for (q in 1:prod) {
        Prod <- base::paste0("bundle_", q)
        names <- c(names, Prod)
      }

      if (!(base::is.null(None))) {
        names <- c(names, "None")
      }

      colnames(df) <- names

      base::rm(names)

      df[, 1] <- c[, id]

      df[base::is.na(df)] <- 0

      for (row in 1:base::nrow(df)) {
        for (q in 1:prod) {
          for (pq in 1:base::length(x[[q]])) {
            df[row, (q + 1)] <- df[row, (q + 1)] + c[row, x[[q]][pq]]
          }
        }
      }

      if (!(base::is.null(None))) {
        df[, base::ncol(df)] <- c[, None]
      }

      if (!(base::is.null(varskeep))) {
        add <- c[, c(id, varskeep)]
        base::colnames(add)[1] <- "ID"
        df <- base::merge(x = df, y = add, by = "ID")
      }

      final_choice <- c[, c(id, choice)]
      base::colnames(final_choice) <- c("ID", "choice")
      df <- base::merge(x = df, y = final_choice, by = "ID")

      .GlobalEnv$HOT <- df
    }

    if (!(base::is.null(price))) {
      c <- data

      if (!(base::is.null(None))) {
        df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1 + 1)))
      }

      if (base::is.null(None)) {
        df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1)))
      }

      names <- c("ID")

      for (q in 1:prod) {
        Prod <- base::paste0("bundle_", q)
        names <- c(names, Prod)
      }

      if (!is.null(None)) {
        names <- c(names, "None")
      }

      base::colnames(df) <- names

      base::rm(names)

      df[, 1] <- c[, id]

      df[base::is.na(df)] <- 0

      for (row in 1:base::nrow(df)) {
        for (q in 1:prod) {
          for (pq in 1:(base::length(x[[q]]) - 1)) {
            df[row, (q + 1)] <- df[row, (q + 1)] + c[row, x[[q]][pq]]

            if (pq == (base::length(x[[q]]) - 1)) {
              price <- base::as.numeric(stats::approx(
                x = c(price_low, price_high),
                y = c(c[row, price_low_po], c[row, price_high_po]), xout = x[[q]][(pq + 1)]
              )[2])

              df[row, (q + 1)] <- df[row, (q + 1)] + price
            }
          }
        }
      }

      if (!(base::is.null(None))) {
        df[, base::ncol(df)] <- c[, None]
      }

      if (!(base::is.null(varskeep))) {
        add <- c[, c(id, varskeep)]
        base::colnames(add)[1] <- "ID"
        df <- base::merge(x = df, y = add, by = "ID")
      }

      final_choice <- c[, c(id, choice)]
      base::colnames(final_choice) <- c("ID", "choice")
      df <- base::merge(x = df, y = final_choice, by = "ID")

      .GlobalEnv$HOT <- df
    }
  }

  if (method == "MaxDiff") {
    c <- data

    if (!(base::is.null(None))) {
      df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1 + 1)))
    }

    if (base::is.null(None)) {
      df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1)))
    }

    names <- c("ID")

    for (q in 1:prod) {
      Prod <- base::paste0("Option_", q)
      names <- c(names, Prod)
    }

    if (!(base::is.null(None))) {
      names <- c(names, "None")
    }

    base::colnames(df) <- names

    base::rm(names)

    df[, 1] <- c[, id]

    df[base::is.na(df)] <- 0

    for (row in 1:base::nrow(df)) {
      for (q in 1:prod) {
        for (pq in 1:base::length(x[[q]])) {
          df[row, (q + 1)] <- c[row, x[[q]]]
        }
      }
    }

    if (!base::is.null(None)) {
      df[, ncol(df)] <- c[, None]
    }

    if (!(base::is.null(varskeep))) {
      add <- c[, c(id, varskeep)]
      base::colnames(add)[1] <- "ID"
      df <- base::merge(x = df, y = add, by = "ID")
    }

    final_choice <- c[, c(id, choice)]
    base::colnames(final_choice) <- c("ID", "choice")
    df <- base::merge(x = df, y = final_choice, by = "ID")

    .GlobalEnv$HOT <- df
  }

  if (method == "ACBC") {
    if (price == "linear") {
      c <- data

      if (!base::is.null(None)) {
        df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1 + 1)))
      }

      if (base::is.null(None)) {
        df <- base::data.frame(matrix(nrow = base::nrow(c), ncol = (prod + 1)))
      }

      names <- c("ID")

      for (q in 1:prod) {
        Prod <- base::paste0("bundle_", q)
        names <- c(names, Prod)
      }

      if (!is.null(None)) {
        names <- c(names, "None")
      }

      base::colnames(df) <- names

      base::rm(names)

      df[, 1] <- c[, id]

      df[base::is.na(df)] <- 0

      for (row in 1:base::nrow(df)) {
        for (q in 1:prod) {
          for (pq in 1:(base::length(x[[q]]) - 1)) {
            df[row, (q + 1)] <- df[row, (q + 1)] + c[row, x[[q]][pq]]

            if (pq == (base::length(x[[q]]) - 1)) {
              price <- base::as.numeric(stats::approx(
                x = c(price_low, price_high),
                y = c(c[row, price_low_po], c[row, price_high_po]), xout = x[[q]][(pq + 1)]
              )[2])

              df[row, (q + 1)] <- df[row, (q + 1)] + price
            }
          }
        }
      }

      if (!base::is.null(None)) {
        df[, ncol(df)] <- c[, None]
      }

      if (!(base::is.null(varskeep))) {
        add <- c[, c(id, varskeep)]
        base::colnames(add)[1] <- "ID"
        df <- base::merge(x = df, y = add, by = "ID")
      }

      final_choice <- c[, c(id, choice)]
      base::colnames(final_choice) <- c("ID", "choice")
      df <- base::merge(x = df, y = final_choice, by = "ID")

      .GlobalEnv$HOT <- df
    }


    if (price == "piecewise") {
      c <- data

      if (!base::is.null(None)) {
        df <- base::data.frame(base::matrix(nrow = base::nrow(c), ncol = (prod + 1 + 1)))
      }

      if (base::is.null(None)) {
        df <- base::data.frame(matrix(nrow = base::nrow(c), ncol = (prod + 1)))
      }

      names <- c("ID")

      for (q in 1:prod) {
        Prod <- base::paste0("bundle_", q)
        names <- c(names, Prod)
      }

      if (!is.null(None)) {
        names <- c(names, "None")
      }

      base::colnames(df) <- names

      base::rm(names)

      df[, 1] <- c[, id]

      df[base::is.na(df)] <- 0

      for (row in 1:base::nrow(df)) {
        for (q in 1:prod) {
          for (pq in 1:(base::length(x[[q]]) - 1)) {
            df[row, (q + 1)] <- df[row, (q + 1)] + c[row, x[[q]][pq]]

            if (pq == (base::length(x[[q]]) - 1)) {
              price <- base::as.numeric(stats::approx(
                x = c(price_low[q], price_high[q]),
                y = c(c[row, price_low_po[q]], c[row, price_high_po[q]]), xout = x[[q]][(pq + 1)]
              )[2])

              df[row, (q + 1)] <- df[row, (q + 1)] + price
            }
          }
        }
      }

      if (!base::is.null(None)) {
        df[, ncol(df)] <- c[, None]
      }

      if (!(base::is.null(varskeep))) {
        add <- c[, c(id, varskeep)]
        base::colnames(add)[1] <- "ID"
        df <- base::merge(x = df, y = add, by = "ID")
      }

      final_choice <- c[, c(id, choice)]
      base::colnames(final_choice) <- c("ID", "choice")
      df <- base::merge(x = df, y = final_choice, by = "ID")

      .GlobalEnv$HOT <- df
    }
  }
}
