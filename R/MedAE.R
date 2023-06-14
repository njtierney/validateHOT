#' Median Absolute Error
#'
#' @description
#' Function to measure the median absolute error (MedAE) of a validation/
#' holdout task.
#'
#' @param data a data frame
#' @param id vector of column index of unique identifier in \code{data}.
#' @param Group optional vector of column number to specify grouping variable
#' to get \code{"medae"} by group.
#' @param opts vector of column indexes of the alternatives included in the
#' validation/ holdout task.
#' @param choice vector of column index of the actual choice.
#'
#' @return a data frame
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom stats median
#' @importFrom labelled is.labelled val_labels
#'
#' @details
#' Median absolute error (MedAE) calculates the deviation between predicted and
#' stated (actual) choice share and returns the median error in the
#' validation/ holdout task, which is less liekly to be influenced by outliers.
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
#' \code{choice} specifies the column index of the actual choice.
#' Input of opts \code{choice} needs to be the column index of actual choice.
#'
#'
#' @examples
#' library(validateHOT)
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
#' medae(data = HOT, id = 1, opts = c(2:9), choice = 10)
#'
#' @examples
#' library(validateHOT)
#' createHOT(
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
#' medae(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)
#'
#' @export


medae <- function(data, id, Group = NULL, opts, choice) {
  if (!base::is.integer(data[[choice]]) & !base::is.numeric(data[[choice]])) {
    base::stop("Error: Choice must be numeric!")
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])) {
    base::warning("Warning: Grouping variable contains NAs.")
  }

  WS <- data[, c(id, Group, choice, opts)]

  Count <- NULL

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

    Predicted <- base::data.frame(base::cbind(
      c(1:base::length(3:(base::ncol(HOT) - 1))),
      c(base::unname(base::colMeans(HOT[, c(3:(base::ncol(HOT) - 1))])))
    ))

    base::colnames(Predicted) <- c("Options", "Pred")

    MedAE <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

    MedAE <- base::merge(x = MedAE, y = Predicted, by = "Options", all.x = T)

    MedAE[base::is.na(MedAE)] <- 0

    MedAEErr <- base::as.data.frame(stats::median(base::abs(MedAE$Share - MedAE$Pred)))

    colnames(MedAEErr) <- "MedAE"
    return(MedAEErr)
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


    MedAE <- base::data.frame(Group = base::character(base::length(base::unique(HOT$Group)) + 1), MedAE = base::numeric(base::length(base::unique(HOT$Group)) + 1))

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


        Predicted <- base::data.frame(base::cbind(
          c(1:base::length(4:(base::ncol(HOT) - 1))),
          c(base::unname(base::colMeans(HOT[, c(4:(base::ncol(HOT) - 1))])))
        ))

        base::colnames(Predicted) <- c("Options", "Pred")

        DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

        DataFrame <- base::merge(x = DataFrame, y = Predicted, by = "Options", all.x = T)

        DataFrame[base::is.na(DataFrame)] <- 0

        MedAE[p, p] <- "All"

        MedAE[p, (p + 1)] <- stats::median(base::abs(DataFrame$Share - DataFrame$Pred))

        base::rm(Helper, Actual, Predicted, DataFrame)
      }

      if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_num <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_num[i])
        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.character(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])
        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.character(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])
        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.factor(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_fac <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::levels(lab_fac)[i])
        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_lab <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])
        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(Group) - 1)), ncol = 1))

      base::colnames(Helper) <- "Options"

      Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))


      Actual <- Group %>%
        dplyr::group_by(choice) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Share = Count / base::sum(Count) * 100) %>%
        base::as.data.frame()

      Predicted <- base::data.frame(base::cbind(
        c(1:base::length(4:(base::ncol(Group) - 1))),
        c(base::unname(base::colMeans(Group[, c(4:(base::ncol(Group) - 1))])))
      ))

      base::colnames(Predicted) <- c("Options", "Pred")

      DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

      DataFrame <- base::merge(x = DataFrame, y = Predicted, by = "Options", all.x = T)

      DataFrame[base::is.na(DataFrame)] <- 0

      MedAE[(p + 1), 1] <- lab[(p + 1)]

      MedAE[(p + 1), 2] <- stats::median(base::abs(DataFrame$Share - DataFrame$Pred))

      base::rm(Helper, Actual, Predicted, DataFrame)

      if (p == base::max(base::length(base::unique(HOT$Group)))) {
        return(MedAE)
      }
    }
  }
}
