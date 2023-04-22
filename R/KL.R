#' Kullback-Leibler Divergence
#'
#' @description Function to measure the Kullback-Leibler Divergence of a holdout task
#' @param data a data frame
#' @param id column index of the \code{id} variable
#' @param Group optional grouping variable to get hit rate by group
#' @param opts column indexes of the options included in the holdout task
#' @param choice column index of the actual choice
#'
#' @return xyz
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff")
#' KL(data = HOT, id = 1, opts = c(2:9), choice = 10)
#'
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff", varskeep = 21)
#' KL(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)
#'
#' @export

KL <- function(data, id, Group = NULL, opts, choice) {

  if (!base::is.integer(data[[choice]]) | !base::is.numeric(data[[choice]])){
    base::stop("Error: Choice must be numeric!")
  }

  WS <- data[, c(id, Group, choice, opts)]

  Count <- NULL

  pred <- NULL

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

    Helper <- base::as.data.frame(base::matrix(nrow = base::length(3:(base::ncol(HOT) - 1)), ncol = 1))

    base::colnames(Helper) <- "Options"

    Helper$Options <- c(1:base::length(3:(base::ncol(HOT) - 1)))

    Actual <- HOT %>%
      dplyr::group_by(choice) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Share = Count / base::sum(Count) * 100) %>%
      base::as.data.frame()

    Predicted <- HOT %>%
      dplyr::group_by(pred) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Predicted = Count / base::sum(Count) * 100) %>%
      base::as.data.frame()


    KL <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

    KL <- base::merge(x = KL, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

    i <- 1:base::length(Helper$Options)

    base::sum(KL[i, 2] * base::log2(KL[i, 2] / KL[i, 3]))

    base::cat("Observed | Predicted: ", (base::sum(KL[i, 2] * base::log2(KL[i, 2] / KL[i, 3])) / 100), "\n")
    base::cat("Predicted | Observed: ", (base::sum(KL[i, 3] * base::log2(KL[i, 3] / KL[i, 2])) / 100))
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

    KL <- base::data.frame(Group = base::character(base::length(base::unique(HOT$Group)) + 1), KL_P_O = base::numeric(base::length(base::unique(HOT$Group)) + 1), KL_O_P = base::numeric(base::length(base::unique(HOT$Group)) + 1))

    for (p in 1:base::length(base::unique(HOT$Group))) {
      if (p == 1) {
        Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(HOT) - 1)), ncol = 1))

        base::colnames(Helper) <- "Options"

        Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))

        Actual <- HOT %>%
          dplyr::group_by(choice) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Share = Count / sum(Count) * 100) %>%
          base::as.data.frame()

        Predicted <- HOT %>%
          dplyr::group_by(pred) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Predicted = Count / base::sum(Count) * 100) %>%
          base::as.data.frame()

        DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

        DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

        i <- 1:base::length(Helper$Options)

        KL[p, p] <- "All"

        KL[p, (p + 1)] <- base::sum(DataFrame[i, 2] * base::log2(DataFrame[i, 2] / DataFrame[i, 3]))
        KL[p, (p + 2)] <- base::sum(DataFrame[i, 3] * base::log2(DataFrame[i, 3] / DataFrame[i, 2]))

        base::rm(Helper, Actual, Predicted, DataFrame)
      }

      Group <- base::subset(HOT, base::as.character(Group) == base::unique(base::as.character(HOT$Group))[p])

      Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(Group) - 1)), ncol = 1))

      base::colnames(Helper) <- "Options"

      Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))


      Actual <- Group %>%
        dplyr::group_by(choice) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Share = Count / base::sum(Count) * 100) %>%
        base::as.data.frame()

      Predicted <- Group %>%
        dplyr::group_by(pred) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Predicted = Count / base::sum(Count) * 100) %>%
        base::as.data.frame()

      DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

      DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

      i <- 1:base::length(Helper$Options)

      KL[(p + 1), 1] <- base::unique(base::as.character(HOT$Group))[p]

      KL[(p + 1), 2] <- base::sum(DataFrame[i, 2] * base::log2(DataFrame[i, 2] / DataFrame[i, 3]))
      KL[(p + 1), 3] <- base::sum(DataFrame[i, 3] * base::log2(DataFrame[i, 3] / DataFrame[i, 2]))

      base::rm(Helper, Actual, Predicted, DataFrame)

      if (p == base::max(base::length(base::unique(HOT$Group)))) {
        KL[, c(2, 3)] <- KL[, c(2, 3)] / 100
        return(KL)
      }
    }
  }
}
