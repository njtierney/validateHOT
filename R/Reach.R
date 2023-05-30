#' Percentage of participants that are reached by assortment
#'
#' @description
#' test
#'
#' @param data data frame including Holdout Options and actual \code{"choice"} and \code{"Group"} if optional argument is defined
#' @param id column index
#' @param Group optional grouping variable to get accuracy by group
#' @param None column index
#' @param method which method should be used, can be either \code{"First choice"} (only the bundle with highest utility is marked as purchase option if its above \code{"None"} utility) or \code{"threshold"} (all bundles with utility above \code{"None"} utility are marked as purchase option)
#' @param bundles column indexes of the bundles included that should be included in the assortment
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19,
#'          id = 1, prod = 7,
#'          prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'          choice = 20, method = "MaxDiff"
#' )
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19,
#'          id = 1, prod = 7,
#'          prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'          choice = 20, method = "MaxDiff", varskeep = 21
#' )
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold", Group = 10)
#' reach(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice", Group = 10)
#'
#' @return a data frame
#' @export

reach <- function(data, id, Group = NULL, None, method, bundles) {
  if (method != "threshold" & method != "First Choice") {
    stop("Error: ", method, " is not valid. Please specify whether to use 'threshold' or 'First Choice'")
  }

  varCheck <- c(bundles, None)

  for (i in 1:base::length(varCheck)) {
    if (!base::is.integer(data[[varCheck[i]]]) & !base::is.numeric(data[[varCheck[i]]])) {
      stop("Error ":colnames(data[varCheck[i]]), " needs to be numeric!")
    }

    if (base::anyNA(data[varCheck[i]])) {
      stop("Error ":colnames(data[[varCheck[i]]]), " has missing values!")
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

      colnames(Reach) <- "reach"

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

      WS_new$reach <- ifelse(rowSums(WS_new[, c(3:ncol(WS_new))]) > 0, 1, 0)

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

      colnames(Reach) <- "reach"

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
