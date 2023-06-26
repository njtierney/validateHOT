#' Root Mean Squared Error
#'
#' @description Function to measure the root mean squared error
#'  of a validation/ holdout task. Calculates the averaged
#'  root mean squared error of the stated and predicted share of alternatives
#'  in the validation/ holdout task.
#'
#'
#' @param data a data frame
#' @param id vector of column index of unique identifier in \code{data}.
#' @param Group optional vector of column number to specify grouping variable
#' to get \code{"rmse"} by group.
#' @param opts vector of column indexes of the alternatives included in the
#' validation/ holdout task.
#' @param choice vector of column index of the actual choice.
#'
#' @return a data frame
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#'
#'
#' @details
#' Root mean squared error (RMSE) calculates the root mean squared error when comparing
#' the share of the actual choice in the holdout task and the predicted share.
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
#' \code{opts} is needed to specify the different alternatives in the validation/ holdout
#' task (also includes the None option).
#' Input of \code{opts} needs to be a vector with column index(es).
#'
#' \code{choice} specifies the column index of the actual choice.
#' Input of opts \code{choice} needs to be the column index of actual choice.
#'
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
#' rmse(data = HOT, id = 1, opts = c(2:9), choice = 10)
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
#' rmse(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)
#' }
#'
#' @export

rmse <- function(data, id, Group = NULL, opts, choice) {
  if (!base::is.integer(data[[choice]]) & !base::is.numeric(data[[choice]])) {
    base::stop("Error: Choice must be numeric!")
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])) {
    base::warning("Warning: Grouping variable contains NAs!")
  }

  if (base::anyNA(data[,opts])) {
    base::stop("Error: opts contains NAs!")
  }

  for (i in 1:length(opts)){
    if(!base::is.numeric(data[, opts[i]])){
      base::stop("Error: opts must be numeric!")
    }
  }

  WS <- data[, c(id, Group, choice, opts)]

  if (base::is.null(Group)) {

    base::colnames(WS) <- c("id", "choice", paste0("Option_", c(1:base::length(opts))))

    WS <- WS %>%
      dplyr::mutate(dplyr::across(
        base::which(base::colnames(WS) == "Option_1"):
          base::which(base::colnames(WS) == base::paste0("Option_", base::length(opts))),
        function(x) 100 * (base::exp(x) / base::rowSums(base::exp(WS[, c(
          base::which(base::colnames(WS) == "Option_1"):
            base::which(base::colnames(WS) == base::paste0("Option_", base::length(opts)))
        )])))
      ),
      pred = base::max.col(WS[,c(base::which(colnames(WS) == "Option_1"):
                                   base::which(colnames(WS) == paste0("Option_", base::length(opts))))]))


    Helper <- tibble::tibble(Options = c(1:base::length(opts)))

    Actual <- tibble::tibble(WS %>%
                               dplyr::group_by(choice) %>%
                               dplyr::summarise(Count = dplyr::n()) %>%
                               dplyr::ungroup() %>%
                               dplyr::mutate(Share = Count / base::sum(Count) * 100)
    )

    Predicted <- tibble::tibble(base::cbind(
      tibble::tibble(c(1:base::length(opts))),
      tibble::tibble(c(base::unname(base::colMeans(WS[,
                                                      c(base::which(colnames(WS) == "Option_1"):
                                                          base::which(colnames(WS) == paste0("Option_", base::length(opts))))])))
      )), .name_repair = ~ c("Options", "Pred")
    )

    RMSE <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

    RMSE <- base::merge(x = RMSE, y = Predicted, by = "Options", all.x = T)

    RMSE[base::is.na(RMSE)] <- 0

    RMSEErr <- tibble::tibble(rmse = base::sqrt((base::sum((base::abs(RMSE$Share - RMSE$Pred))^2) / base::length(opts))))


    return(RMSEErr)
  }

  if (!(base::is.null(Group))) {
    base::colnames(WS) <- c("id", "Group", "choice", paste0("Option_", c(1:base::length(opts))))

    WS <- WS %>%
      dplyr::mutate(dplyr::across(
        base::which(base::colnames(WS) == "Option_1"):
          base::which(base::colnames(WS) == base::paste0("Option_", base::length(opts))),
        function(x) 100 * (base::exp(x) / base::rowSums(base::exp(WS[, c(
          base::which(base::colnames(WS) == "Option_1"):
            base::which(base::colnames(WS) == base::paste0("Option_", base::length(opts)))
        )])))
      ),
      pred = base::max.col(WS[,c(base::which(colnames(WS) == "Option_1"):
                                   base::which(colnames(WS) == paste0("Option_", base::length(opts))))]))


    RMSE <- tibble::tibble(Group = base::character(base::length(base::unique(WS$Group)) + 1), rmse = base::numeric(base::length(base::unique(WS$Group)) + 1))

    for (p in 1:base::length(base::unique(WS$Group))) {
      if (p == 1) {

        Helper <- tibble::tibble(Options = c(1:base::length(opts)))

        Actual <- tibble::tibble(
          WS %>%
            dplyr::group_by(choice) %>%
            dplyr::summarise(Count = dplyr::n()) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Share = Count / sum(Count) * 100))

        Predicted <- tibble::tibble(base::cbind(
          tibble::tibble(c(1:base::length(opts))),
          tibble::tibble(c(base::unname(base::colMeans(WS[,
                                                          c(base::which(colnames(WS) == "Option_1"):
                                                              base::which(colnames(WS) == paste0("Option_", base::length(opts))))])))
          )), .name_repair = ~ c("Options", "Pred")
        )

        DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

        DataFrame <- base::merge(x = DataFrame, y = Predicted, by = "Options", all.x = T)

        DataFrame[base::is.na(DataFrame)] <- 0

        RMSE[p, p] <- "All"

        RMSE[p, (p + 1)] <- base::sqrt((base::sum((base::abs(DataFrame$Share - DataFrame$Pred))^2) / base::length(opts)))

        base::rm(Helper, Actual, Predicted, DataFrame)
      }

      if (base::is.numeric(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_num <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_num[i])
        }

        Group <- base::subset(WS, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])
        }

        Group <- base::subset(WS, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.character(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])
        }

        Group <- base::subset(WS, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.factor(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_fac <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::levels(lab_fac)[i])
        }

        Group <- base::subset(WS, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_lab <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])
        }

        Group <- base::subset(WS, Group == base::sort(base::unique(WS$Group))[p])
      }

      Helper <- tibble::tibble(Options = c(1:base::length(opts)))

      Actual <- tibble::tibble(
        Group %>%
          dplyr::group_by(choice) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Share = Count / base::sum(Count) * 100)
      )

      Predicted <- tibble::tibble(base::cbind(
        tibble::tibble(c(1:base::length(opts))),
        tibble::tibble(c(base::unname(base::colMeans(Group[,
                                                           c(base::which(colnames(Group) == "Option_1"):
                                                               base::which(colnames(Group) == paste0("Option_", base::length(opts))))])))
        )), .name_repair = ~ c("Options", "Pred")
      )

      DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

      DataFrame <- base::merge(x = DataFrame, y = Predicted, by = "Options", all.x = T)

      DataFrame[base::is.na(DataFrame)] <- 0

      RMSE[(p + 1), 1] <- lab[(p + 1)]

      RMSE[(p + 1), 2] <- base::sqrt((base::sum((base::abs(DataFrame$Share - DataFrame$Pred))^2) / base::length(opts)))

      base::rm(Helper, Actual, Predicted, DataFrame)

      if (p == base::max(base::length(base::unique(WS$Group)))) {
        return(RMSE)
      }
    }
  }
}
