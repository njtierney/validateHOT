#' Averaged number of times a person is reached by a specific assortment of bundles
#'
#' @description
#' Frequency function of TURF analysis to measure the average time a consumer
#' is reached with a specific product bundle assortment. Can either be calculated
#' using 'First Choice' or 'threshold' rule. See Details section for more
#' information and specifying the data in the correct way.
#'
#' @param data data frame including alternatives in the validation task and actual \code{"choice"}. \code{"None"} alternative needs to be included.
#' @param id column index of \code{"id"}
#' @param Group optional grouping variable to get results by group
#' @param bundles column indexes of the bundles that should be included in the assortment
#' @param None column index of None alternative
#' @param method either \code{"First choice"} or \code{"threshold"}, please see Details

#'
#' @details
#' Please specify the method you are using. The method can be either set
#' to \code{"First choice"} (only the bundle with highest utility is marked as purchase option if
#' its above \code{"None"} utility) or \code{"threshold"}
#' (all bundles with utility above \code{"None"} utility are marked as purchase option).
#'
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#'
#' @examples
#' library(validateHOT)
#' data("MaxDiff")
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
#' freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold")
#' freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice")
#'
#' @examples
#' library(validateHOT)
#' data("MaxDiff")
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
#' freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "threshold", Group = 10)
#' freqassort(data = HOT, id = 1, bundles = c(2, 3, 7), None = 9, method = "First Choice", Group = 10)
#'
#' @return a data frame
#' @export
#'
freqassort <- function(data, id, Group = NULL, None, method = c("threshold" | "First Choice"), bundles) {
  if (method != "threshold" & method != "First Choice") {
    stop("Error: ", method, " is not valid. Please specify whether to use 'threshold' or 'First Choice'")
  }

  varCheck <- c(bundles, None)

  for (i in 1:base::length(varCheck)) {
    if (!base::is.integer(data[[varCheck[i]]]) & !base::is.numeric(data[[varCheck[i]]])) {
      stop("Error: ", colnames(data[varCheck[i]]), " needs to be numeric!")
    }
  }

  for (i in 1:base::length(varCheck)) {
    if (base::anyNA(data[varCheck[i]])) {
      stop("Error: ", colnames(data[[varCheck[i]]]), " has missing values!")
    }
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])) {
    base::warning("Warning: Grouping variable contains NAs.")
  }

  freq <- NULL

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

      Freq <- base::as.data.frame(base::mean(base::rowSums(WS_new[, c(2:base::ncol(WS_new))])))

      colnames(Freq) <- "Frequency"

      return(Freq)
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

      WS_new$freq <- base::rowSums(WS_new[, c(3:base::ncol(WS_new))])

      Frequency <- base::rbind(
        WS_new %>%
          dplyr::summarise(
            Group = "All",
            Frequency = base::mean(freq)
          ) %>%
          base::as.data.frame(),
        WS_new %>%
          dplyr::group_by(Group) %>%
          dplyr::summarise(
            Frequency = base::mean(freq)
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

      Frequency$Group <- lab

      return(Frequency)
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

      Freq <- base::as.data.frame(base::mean(base::rowSums(WS_new[, c(2:base::ncol(WS_new))])))

      colnames(Freq) <- "Frequency"

      return(Freq)
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



      WS_new$freq <- base::rowSums(WS_new[, c(3:base::ncol(WS_new))])

      Frequency <- base::rbind(
        WS_new %>%
          dplyr::summarise(
            Group = "All",
            Frequency = base::mean(freq)
          ) %>%
          base::as.data.frame(),
        WS_new %>%
          dplyr::group_by(Group) %>%
          dplyr::summarise(
            Frequency = base::mean(freq)
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

      Frequency$Group <- lab

      return(Frequency)
    }
  }
}
