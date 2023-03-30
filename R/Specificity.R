#' Specificity
#'
#' @description test
#'
#' @param data data frame including Holdout Options and actual \code{"choice"} and \code{"Group"} if optional argument is defined
#' @param id column index
#' @param Group optional grouping variable to get accuracy by group
#' @param opts column indexes of the options included in the holdout task
#' @param choice column index of the actual choice
#' @param None column index
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#'
#' @return test
#' @export

Specificity <- function(data, id, Group = NULL, opts, choice, None) {
  WS <- data[, c(id, Group, choice, opts)]

  buy <- pred_buy <- NULL

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



    HOT$buy <- base::ifelse(HOT$choice != base::match(None, opts), 1, 2)
    HOT$pred_buy <- base::ifelse(HOT$pred != base::match(None, opts), 1, 2)

    return(HOT %>%
      dplyr::summarize(
        Specificity = base::round(100 * (base::sum(buy == 2 & pred_buy == 2) / (base::sum(buy == 2 & pred_buy == 2) + base::sum(buy == 2 & pred_buy == 1))), digits = 2)
      ))
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
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 4)])) * 100
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

    HOT$buy <- base::ifelse(HOT$choice != base::match(None, opts), 1, 2)
    HOT$pred_buy <- base::ifelse(HOT$pred != base::match(None, opts), 1, 2)

    return(HOT %>%
      dplyr::group_by(Group) %>%
      dplyr::summarize(
        Specificity = base::round(100 * (base::sum(buy == 2 & pred_buy == 2) / (base::sum(buy == 2 & pred_buy == 2) + base::sum(buy == 2 & pred_buy == 1))), digits = 2)
      ))
  }
}
