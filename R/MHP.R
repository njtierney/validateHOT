#' MHP
#'
#' @description Function to measure the mean hit probability of a holdout task
#' @param data a data frame
#' @param id column index of the \code{id} variable
#' @param Group optional grouping variable to get hit rate by group
#' @param opts column indexes of the options included in the holdout task
#' @param choice column index of the actual choice
#'
#' @return xyz
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#'
#' @export
MHP <- function(data, id, Group = NULL, opts, choice) {
  WS <- data[, c(id, Group, choice, opts)]

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

    HOT$MHP <- 0
    for (i in 1:base::nrow(HOT)) {
      HOT$MHP[i] <- HOT[i, (HOT$choice[i] + 2)]
    }

    base::print(base::mean(HOT$MHP))
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

    HOT$MHP <- 0
    for (i in 1:base::nrow(HOT)) {
      HOT$MHP[i] <- HOT[i, (HOT$choice[i] + 3)]
    }


    return(HOT %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(MeanHitProb = base::mean(MHP)) %>%
      base::as.data.frame())
  }
}
