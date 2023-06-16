#' Percentage of participants that are reached by assortment
#'
#' @description
#' Reach function of TURF analysis to measure the number of  the averaged percentage of how many participants you can reach
#' (at least one of the products resemble a purchase option) is reached with a specific product bundle assortment. Can either be calculated
#' using 'First Choice' or 'threshold' rule. See Details section for more
#' information and specifying the data in the correct way.
#'
#' @param data data data frame with all relevant variables.
#' @param id vector of column index of unique identifier in \code{data}.
#' @param Group optional vector of column number to specify grouping variable
#' to get \code{"reach"} by group.
#' @param bundles vector of column indexes of the bundles that should be included in the assortment.
#' @param None vector of column index of None alternative.
#' @param method character variable that needs to be specified to either \code{"First choice"} or \code{"threshold"}, please see Details.
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#'
#' @details
#' Reach measures the averaged percentage of how many participants you can reach (at least one of the products resemble a purchase option)
#' product assortment you are testing. The current logic of \code{reach()}
#' is that it needs to exceed a threshold. In the case of \code{reach()}
#' this threshold is referred to the \code{None} argument in \code{data}.
#'
#' \code{reach()} currently provides two methods:
#' \itemize{
#' \item \code{"First Choice"}: Only the alternative with the highest utility is considered,
#' if its utility is above the utility of \code{None}, it is marked as potential purchase option.
#' \item \code{"threshold"}: All alternatives are considered. if utility of alternative is
#' larger than the utility of \code{None}, it is marked as potential purchase option.
#' }
#'
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
#' \code{bundles} is needed to specify the different alternatives in the
#' product assortment that should be considered.
#' Input of \code{bundles} needs to be a vector with column index(es).
#'
#' \code{None} specifies the column index of the \code{None} alternative. Is used as
#' threshold to determine whether alternative resembles a purchase option or not.
#' Needs to be specified for \code{reach()}.
#'
#' \code{method} character variable whether \code{method = "First Choice"} or
#' \code{method = "threshold"} should be applied to calculate \code{reach()}.
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
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")
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
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold", Group = 10)
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice", Group = 10)
#' }
#'
#' @return a data frame
#' @export

reach <- function(data, id, Group = NULL, None, method, bundles) {
  if (method != "threshold" & method != "First Choice") {
    base::stop("Error: ", method, " is not valid. Please specify whether to use 'threshold' or 'First Choice'")
  }

  varCheck <- c(bundles, None)

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

  reach <- NULL

  if (method == "threshold") {
    if (base::is.null(Group)) {
      WS <- data[, c(id, bundles, None)]

      names <- c("id")
      for (j in 1:base::length(bundles)) {
        nj <- base::paste0("Bundle_", j)
        names <- c(names, nj)
      }
      names <- c(names, "None")

      base::colnames(WS) <- names

      WS_new <- base::data.frame(base::matrix(nrow = base::nrow(WS), ncol = (base::ncol(WS) - 1)))

      base::colnames(WS_new) <- names[-base::length(names)]

      WS_new[, 1] <- WS[, 1]

      for (i in 1:base::nrow(WS_new)) {
        for (j in 2:base::ncol(WS_new)) {
          WS_new[i, j] <- base::ifelse(WS[i, (j)] > WS[i, "None"], 1, 0)
        }
      }

      Reach <- base::as.data.frame(base::mean(base::ifelse(base::rowSums(WS_new[, c(2:base::ncol(WS_new))]) > 0, 1, 0)) * 100)

      base::colnames(Reach) <- "reach"

      return(Reach)
    }

    if (!(base::is.null(Group))) {
      WS <- data[, c(id, Group, bundles, None)]

      names <- c("id", "Group")
      for (j in 1:base::length(bundles)) {
        nj <- base::paste0("Bundle_", j)
        names <- c(names, nj)
      }
      names <- c(names, "None")

      base::colnames(WS) <- names

      WS_new <- base::data.frame(base::matrix(nrow = base::nrow(WS), ncol = (base::ncol(WS) - 1)))

      base::colnames(WS_new) <- names[-base::length(names)]

      WS_new[, c(1, 2)] <- WS[, c(1, 2)]


      for (i in 1:base::nrow(WS_new)) {
        for (j in 3:base::ncol(WS_new)) {
          WS_new[i, j] <- base::ifelse(WS[i, (j)] > WS[i, "None"], 1, 0)
        }
      }

      WS_new$reach <- base::ifelse(base::rowSums(WS_new[, c(3:base::ncol(WS_new))]) > 0, 1, 0)

      Reach <- base::rbind(
        WS_new %>%
          dplyr::summarise(
            Group = "All",
            reach = base::mean(reach) * 100
          ) %>%
          base::as.data.frame(),
        WS_new %>%
          dplyr::group_by(Group) %>%
          dplyr::summarise(
            reach = base::mean(reach) * 100
          ) %>%
          base::as.data.frame()
      )

      # fixing grouping variable

      lab <- c()

      if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_num <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_num[i])
        }
      }

      if (base::is.character(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])
        }
      }


      if (base::is.factor(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_fac <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::levels(lab_fac)[i])
        }
      }

      if (labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_lab <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])
        }
      }

      Reach$Group <- lab

      return(Reach)
    }
  }

  if (method == "First Choice") {
    if (base::is.null(Group)) {
      WS <- data[, c(id, bundles, None)]

      names <- c("id")
      for (j in 1:base::length(bundles)) {
        nj <- base::paste0("Bundle_", j)
        names <- c(names, nj)
      }
      names <- c(names, "None")

      base::colnames(WS) <- names

      WS_new <- base::data.frame(base::matrix(nrow = base::nrow(WS), ncol = (base::ncol(WS) - 1)))

      base::colnames(WS_new) <- names[-base::length(names)]

      WS_new[, 1] <- WS[, 1]

      for (i in 1:base::nrow(WS_new)) {
        for (k in 2:base::ncol(WS_new)) {
          WS_new[i, k] <- base::ifelse((WS[i, k] == base::max(WS[i, 2:(base::ncol(WS) - 1)]) & (base::max(WS[i, 2:(base::ncol(WS) - 1)]) > WS[i, "None"])), 1, 0)
        }
      }

      Reach <- base::as.data.frame(base::mean(base::ifelse(base::rowSums(WS_new[, c(2:base::ncol(WS_new))]) > 0, 1, 0)) * 100)

      base::colnames(Reach) <- "reach"

      return(Reach)
    }

    if (!(base::is.null(Group))) {
      WS <- data[, c(id, Group, bundles, None)]

      names <- c("id", "Group")
      for (j in 1:base::length(bundles)) {
        nj <- base::paste0("Bundle_", j)
        names <- c(names, nj)
      }
      names <- c(names, "None")

      base::colnames(WS) <- names

      WS_new <- base::data.frame(base::matrix(nrow = base::nrow(WS), ncol = (base::ncol(WS) - 1)))

      base::colnames(WS_new) <- names[-base::length(names)]

      WS_new[, c(1, 2)] <- WS[, c(1, 2)]


      for (i in 1:base::nrow(WS_new)) {
        for (k in 3:base::ncol(WS_new)) {
          WS_new[i, k] <- base::ifelse((WS[i, k] == base::max(WS[i, 3:(base::ncol(WS) - 1)]) & (base::max(WS[i, 3:(base::ncol(WS) - 1)]) > WS[i, "None"])), 1, 0)
        }
      }



      WS_new$reach <- base::ifelse(base::rowSums(WS_new[, c(3:base::ncol(WS_new))]) > 0, 1, 0)

      Reach <- base::rbind(
        WS_new %>%
          dplyr::summarise(
            Group = "All",
            reach = base::mean(reach) * 100
          ) %>%
          base::as.data.frame(),
        WS_new %>%
          dplyr::group_by(Group) %>%
          dplyr::summarise(
            reach = base::mean(reach) * 100
          ) %>%
          base::as.data.frame()
      )

      # fixing grouping variable

      lab <- c()

      if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_num <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_num[i])
        }
      }

      if (base::is.character(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])
        }
      }


      if (base::is.factor(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_fac <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::levels(lab_fac)[i])
        }
      }

      if (labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_lab <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])
        }
      }

      Reach$Group <- lab

      return(Reach)
    }
  }
}
