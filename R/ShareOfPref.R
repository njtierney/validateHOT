#' Median Absolute Error
#'
#' @description
#' Function to measure the Median absolute error of a holdout task
#'
#' @param data a data frame
#' @param id column index of the \code{id} variable
#' @param Group optional grouping variable to get hit rate by group
#' @param opts column indexes of the options included in the holdout task
#' @param choice column index of the actual choice
#'
#' @return xyz
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom stats qt sd
#'
#' @export

ShareofPref <- function(data, id, Group = NULL, opts, choice) {
  WS <- data[, c(id, Group, choice, opts)]

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

    base::colnames(WS) <- c("id", "choice", Options)

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


    HOT <- WS[, c("id", "choice", Perc)]

    HOT$pred <- 0

    for (i in 1:base::nrow(HOT)) {
      for (k in 3:(base::ncol(HOT) - 1)) {
        if (HOT[i, k] == base::max(HOT[i, 3:(base::ncol(HOT) - 1)])) {
          HOT$pred[i] <- k - 2
        }
      }
    }

    MW <- unname(colMeans(HOT[, c(3:(base::ncol(HOT) - 1))]))

    Options <- c()

    for (i in 3:(base::length(HOT) - 1)) {
      Options <- c(Options, base::paste0("Option ", i - 2))
    }

    MarketShare <- base::data.frame(base::matrix(nrow = base::length(Options), ncol = 4))

    MarketShare[base::is.na(MarketShare)] <- 0

    base::colnames(MarketShare) <- c("Options", "Mean", "Lower CI", "Upper CI")

    MarketShare[, 1] <- Options

    MarketShare[, 2] <- MW

    for (i in 1:base::nrow(MarketShare)) {
      m <- MarketShare[i, 2]
      s <- stats::sd(HOT[, (i + 2)])
      n <- base::nrow(HOT)

      margin <- stats::qt(0.975, df = n - 1) * s / sqrt(n)

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

    base::colnames(WS) <- c("id", "Group", "choice", Options)

    for (i in 1:base::length(newNames)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 4:(base::ncol(WS) - base::length(opts))) {
      WS[, (base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 3)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 3)])) * 100
    }


    HOT <- WS[, c("id", "choice", "Group", Perc)]

    HOT$pred <- 0

    for (i in 1:base::nrow(HOT)) {
      for (k in 4:(base::ncol(HOT) - 1)) {
        if (HOT[i, k] == base::max(HOT[i, 4:(base::ncol(HOT) - 1)])) {
          HOT$pred[i] <- k - 3
        }
      }
    }

    for (t in 1:base::length(base::unique(HOT$Group))) {
      Sub <- base::subset(HOT, base::as.character(Group) == base::unique(base::as.character(HOT$Group))[t])

      MW <- unname(colMeans(Sub[, c(4:(base::ncol(Sub) - 1))]))

      Options <- c()

      for (i in 4:(base::length(Sub) - 1)) {
        Options <- c(Options, base::paste0("Option ", i - 3))
      }

      MarketShare <- base::data.frame(base::matrix(nrow = base::length(Options), ncol = 4))

      MarketShare[base::is.na(MarketShare)] <- 0

      base::colnames(MarketShare) <- c("Options", "Mean", "Lower CI", "Upper CI")

      MarketShare[, 1] <- Options

      MarketShare[, 2] <- MW

      for (l in 1:base::nrow(MarketShare)) {
        m <- MarketShare[l, 2]
        s <- stats::sd(Sub[, (l + 3)])
        n <- base::nrow(Sub)

        margin <- stats::qt(0.975, df = n - 1) * s / sqrt(n)

        MarketShare[l, 3] <- m - margin

        MarketShare[l, 4] <- m + margin
      }
      cat("\nShare of Preferences ", base::unique(base::as.character(HOT$Group))[t], ":\n", sep = "")


      print(MarketShare)
    }
  }
}
