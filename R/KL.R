#' Kullback-Leibler Divergence
#'
#' @description Function to measure the Kullback-Leibler Divergence of a validation task
#' @param data data frame with all relevant variables.
#' @param id vector of column index of unique identifier in \code{data}.
#' @param Group optional vector of column number to specify grouping variable
#' to get \code{"kl"} by group.
#' @param opts vector of column indexes of the alternatives included in the
#' validation/holdout task.
#' @param choice vector of column index of the actual choice.
#' @param epsilon vector of noise that should be added to 0 values, per default set to 1e-05
#'
#' @return a data frame
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#'
#' @details
#' Kullback-Leibler-Divergence which measures the divergence between the actual choice distribution and the predicted
#' choice distribution (Ding et al., 2011; Drost, 2018). Currently only provides
#' the deviation measured based on \eqn{log{_2}} algorithm.
#'
#' Due to the asymmetry of the Kullback-Leibler divergence, output provides both
#' \code{"KL_O_P"} which is equivalent to (Observed || Predicted) and
#' \code{"KL_P_O"} which is equivalent to (Predicted || Observed).
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
#' \code{opts} is needed to specify the different alternatives in the validation/holdout
#' task (also includes the None option).
#' Input of \code{opts} needs to be a vector with column index(es).
#'
#' \code{choice} specifies the column index of the actual choice.
#' Input of opts \code{choice} needs to be the column index of actual choice.
#'
#' \code{epsilon} needs to be a numeric input in case of 0 in the numerator or denominator. 0
#' then will be replaced by \code{epsilon}. Default value is \code{epsilon = .1e-5}, however, can
#' be adopted.
#'
#' @references {
#'
#' Ding, Min, John R. Hauser, Songting Dong, Daria Dzyabura, Zhilin Yang, SU Chenting, and Steven P. Gaskin. (2011).
#' Unstructured Direct Elicitation of Decision Rules. \emph{Journal of Marketing Research 48}(1): 116-27. \verb{https://doi.org/10.1509/jmkr.48.1.116}.
#'
#' Drost, Hajk-Georg. (2018). Philentropy: Information Theory and Distance Quantification with R. \emph{Journal of Open Source Software 3}(26), 765, \verb{https://joss.theoj.org/papers/10.21105/joss.00765}.
#'
#'
#' }
#'
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
#' kl(data = HOT, id = 1, opts = c(2:9), choice = 10)
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
#' kl(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)
#' }
#'
#' @export

kl <- function(data, id, Group = NULL, opts, choice, epsilon = NULL) {
  if (base::is.null(epsilon)) {
    epsilon <- .00001
  }

  if (!base::is.integer(data[[choice]]) & !base::is.numeric(data[[choice]])) {
    base::stop("Error: Choice must be numeric!")
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])) {
    base::warning("Warning: Grouping variable contains NAs.")
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
      dplyr::mutate(Share = Count / base::sum(Count)) %>%
      base::as.data.frame()

    Predicted <- HOT %>%
      dplyr::group_by(pred) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Predicted = Count / base::sum(Count)) %>%
      base::as.data.frame()


    KL <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

    KL <- base::merge(x = KL, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

    KL[base::is.na(KL)] <- epsilon

    i <- 1:base::length(Helper$Options)

    base::sum(KL[i, 2] * base::log2(KL[i, 2] / KL[i, 3]))

    Res <- base::as.data.frame(base::cbind(base::sum(KL[i, 2] * base::log2(KL[i, 2] / KL[i, 3])), base::sum(KL[i, 3] * base::log2(KL[i, 3] / KL[i, 2]))))

    base::colnames(Res) <- c("KL_O_P", "KL_P_O")

    return(Res)
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

    KL <- base::data.frame(Group = base::character(base::length(base::unique(HOT$Group)) + 1), KL_O_P = base::numeric(base::length(base::unique(HOT$Group)) + 1), KL_P_O = base::numeric(base::length(base::unique(HOT$Group)) + 1))

    for (p in 1:base::length(base::unique(HOT$Group))) {
      if (p == 1) {
        Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(HOT) - 1)), ncol = 1))

        base::colnames(Helper) <- "Options"

        Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))

        Actual <- HOT %>%
          dplyr::group_by(choice) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Share = Count / sum(Count)) %>%
          base::as.data.frame()

        Predicted <- HOT %>%
          dplyr::group_by(pred) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Predicted = Count / base::sum(Count)) %>%
          base::as.data.frame()

        DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

        DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

        i <- 1:base::length(Helper$Options)

        KL[p, p] <- "All"

        KL[p, (p + 1)] <- base::sum(DataFrame[i, 2] * base::log2(DataFrame[i, 2] / DataFrame[i, 3]))
        KL[p, (p + 2)] <- base::sum(DataFrame[i, 3] * base::log2(DataFrame[i, 3] / DataFrame[i, 2]))

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
        dplyr::mutate(Share = Count / base::sum(Count)) %>%
        base::as.data.frame()

      Predicted <- Group %>%
        dplyr::group_by(pred) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Predicted = Count / base::sum(Count)) %>%
        base::as.data.frame()

      DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

      DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

      DataFrame[base::is.na(DataFrame)] <- epsilon

      i <- 1:base::length(Helper$Options)

      KL[(p + 1), 1] <- lab[(p + 1)]

      KL[(p + 1), 2] <- base::sum(DataFrame[i, 2] * base::log2(DataFrame[i, 2] / DataFrame[i, 3]))
      KL[(p + 1), 3] <- base::sum(DataFrame[i, 3] * base::log2(DataFrame[i, 3] / DataFrame[i, 2]))

      base::rm(Helper, Actual, Predicted, DataFrame)

      if (p == base::max(base::length(base::unique(HOT$Group)))) {
        return(KL)
      }
    }
  }
}
