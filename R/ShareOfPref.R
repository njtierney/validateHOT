#' Share of Preferences of Options included in HOT
#'
#' @description
#' Function to measure the share of preferences of each option in the validation task
#'
#' @param data a data frame
#' @param id vector of column index of unique identifier in \code{data}.
#' @param Group optional vector of column number to specify grouping variable
#' to get \code{"shareofpref"} by group.
#' @param opts vector of column indexes of the alternatives included in the
#' validation/ holdout task.
#'
#' @return a data frame; a list if \code{Group} is specified
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom labelled is.labelled val_labels
#'
#' @details
#' Share of Preference provides the aggrgated share of each alternative in the
#' validation/ holdout task as well as the lower and upper confidence interval
#' of each alternative which is calculated according to the following formula
#' \eqn{mean +/- 1.96 x \frac{sd}{\sqrt(n)}} (Orme, 2020, p. 94).
#'
#' \code{data} needs to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()} function.
#'
#' \code{id} needs to be the column index of the id (unique for each participant)
#' in \code{data}.
#'
#' \code{Group} optional Grouping variable, if results should be display by different conditions.
#' Input of \code{Group} needs to be a vector of the column index of \code{Group}.
#'
#' \code{opts} is needed to specify the different alternatives in the validation/ holdout
#' task (also includes the None option).
#' Input of \code{opts} needs to be a vector with column index(es).
#'
#'
#' @references {
#'
#' Orme, B. K. (2020). \emph{Getting Started with Conjoint Analysis:
#' Strategies for Product Design and Pricing Research}. 4th edition.
#' Manhattan Beach, CA: Research Publishers LLC.
#'
#' }
#'
#' @examples
#' \dontrun{
#' HOT <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   None = 19,
#'   prod = 7,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20
#' )
#'
#' shareofpref(data = HOT, id = 1, opts = c(2:9))
#' }
#'
#' @examples
#' \dontrun{
#' HOT <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   None = 19,
#'   prod = 7,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   varskeep = 21,
#'   choice = 20
#' )
#'
#' shareofpref(data = HOT, id = 1, opts = c(2:9), Group = 10)
#' }
#'
#' @export

shareofpref <- function(data, id, Group = NULL, opts) {
  varCheck <- c(opts)

  for (i in 1:base::length(varCheck)) {
    if (!base::is.integer(data[[varCheck[i]]]) & !base::is.numeric(data[[varCheck[i]]])) {
      base::stop("Error: ", base::colnames(data[varCheck[i]]), " needs to be numeric!")
    }
  }

  for (i in 1:base::length(varCheck)) {
    if (base::anyNA(data[varCheck[i]])) {
      base::stop("Error: ", base::colnames(data[[varCheck[i]]]), " has missing values!")
    }
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])) {
    base::warning("Warning: Grouping variable contains NAs.")
  }

  WS <- data[, c(id, Group, opts)]

  Count <- sd <- NULL

  if (base::is.null(Group)) {
    Options <- c()

    for (k in 1:base::length(opts)) {
      name <- base::paste0("Option_", k)
      Options <- c(Options, name)
    }

    newNames <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Opt_", k)
      newNames <- c(newNames, name)
    }

    Perc <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Perc_", k)
      Perc <- c(Perc, name)
    }

    base::colnames(WS) <- c("id", Options)

    for (i in 1:base::length(newNames)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 2:(base::ncol(WS) - base::length(opts))) {
      WS[, (base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 2):(base::length(opts) + base::length(opts) + 1)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 2):(base::length(opts) + base::length(opts) + 1)])) * 100
    }


    HOT <- WS[, c("id", Perc)]


    MW <- base::unname(base::colMeans(HOT[, c(2:(base::ncol(HOT)))]))

    Options <- c()

    for (i in 2:(base::length(HOT))) {
      Options <- c(Options, base::paste0("Option ", i - 1))
    }

    MarketShare <- base::data.frame(base::matrix(nrow = base::length(Options), ncol = 4))

    MarketShare[base::is.na(MarketShare)] <- 0

    base::colnames(MarketShare) <- c("Options", "Mean", "Lower CI", "Upper CI")

    MarketShare[, 1] <- Options

    MarketShare[, 2] <- MW

    for (i in 1:base::nrow(MarketShare)) {
      m <- MarketShare[i, 2]
      s <- stats::sd(HOT[, (i + 1)])
      n <- base::nrow(HOT)

      margin <- 1.96 * (s / base::sqrt(n))

      MarketShare[i, 3] <- m - margin

      MarketShare[i, 4] <- m + margin
    }

    return(MarketShare)
  }

  if (!(base::is.null(Group))) {
    Options <- c()

    for (k in 1:base::length(opts)) {
      name <- base::paste0("Option_", k)
      Options <- c(Options, name)
    }

    newNames <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Opt_", k)
      newNames <- c(newNames, name)
    }

    Perc <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Perc_", k)
      Perc <- c(Perc, name)
    }

    base::colnames(WS) <- c("id", "Group", Options)

    for (i in 1:base::length(newNames)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 3:(base::ncol(WS) - base::length(opts))) {
      WS[, (base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 3):(base::length(opts) + base::length(opts) + 2)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 3):(base::length(opts) + base::length(opts) + 2)])) * 100
    }


    HOT <- WS[, c("id", "Group", Perc)]

    lab <- c()

    output <- base::list()

    for (t in 1:base::length(base::unique(HOT$Group))) {
      if (t == 1) {
        MW <- base::unname(base::colMeans(HOT[, c(3:(base::ncol(HOT)))]))

        MarketShare_ALL <- base::data.frame(base::matrix(nrow = base::length(Options), ncol = 4))

        MarketShare_ALL[base::is.na(MarketShare_ALL)] <- 0

        base::colnames(MarketShare_ALL) <- c("Options", "Mean", "Lower CI", "Upper CI")

        MarketShare_ALL[, 1] <- Options

        MarketShare_ALL[, 2] <- MW

        for (all in 1:base::nrow(MarketShare_ALL)) {
          m <- MarketShare_ALL[all, 2]
          s <- stats::sd(HOT[, (all + 2)])
          n <- base::nrow(HOT)

          margin <- 1.96 * (s / base::sqrt(n))

          MarketShare_ALL[all, 3] <- m - margin

          MarketShare_ALL[all, 4] <- m + margin
        }

        output[[t]] <- MarketShare_ALL
      }


      if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)) {
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_num <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_num[i])
        }

        Sub <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[t])
      }

      if (base::is.character(WS$Group)) {
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])
        }

        Sub <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[t])
      }


      if (base::is.factor(WS$Group)) {
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_fac <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::levels(lab_fac)[i])
        }

        Sub <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[t])
      }

      if (labelled::is.labelled(WS$Group)) {
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_lab <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])
        }

        Sub <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[t])
      }

      MW <- unname(colMeans(Sub[, c(3:(base::ncol(Sub)))]))

      Options <- c()

      for (i in 3:(base::length(Sub))) {
        Options <- c(Options, base::paste0("Option ", i - 2))
      }

      MarketShare <- base::data.frame(base::matrix(nrow = base::length(Options), ncol = 4))

      MarketShare[base::is.na(MarketShare)] <- 0

      base::colnames(MarketShare) <- c("Options", "Mean", "Lower CI", "Upper CI")

      MarketShare[, 1] <- Options

      MarketShare[, 2] <- MW

      for (l in 1:base::nrow(MarketShare)) {
        m <- MarketShare[l, 2]
        s <- stats::sd(Sub[, (l + 2)])
        n <- base::nrow(Sub)

        margin <- 1.96 * (s / base::sqrt(n))

        MarketShare[l, 3] <- m - margin

        MarketShare[l, 4] <- m + margin
      }

      output[[(t + 1)]] <- MarketShare

      if (t == base::length(base::unique(HOT$Group))) {
        base::names(output) <- c("All", lab[1:base::length(base::unique(HOT$Group))])
        return(output)
      }
    }
  }
}
