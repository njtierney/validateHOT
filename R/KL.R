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
#' @param basis character string to define the logarithm base, currently can choose between \code{log} and \code{log2}
#'
#' @return a data frame
#' @importFrom dplyr group_by summarise reframe n ungroup
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#' @importFrom tibble tibble
#'
#' @details
#' Kullback-Leibler-Divergence which measures the divergence between the actual choice distribution and the predicted
#' choice distribution (Ding et al., 2011; Drost, 2018). Currently only provides
#' the deviation measured based on \eqn{log} and \eqn{log{_2}} algorithm.
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
#' \code{basis} needs to be a character string, deciding which logarithm base you want to apply
#' to calculate Kullback-Leibler. You can choose between \eqn{log} and \eqn{log{_2}}
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
#' kl(data = HOT, id = 1, opts = c(2:9), choice = 10, basis = "log2")
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
#' kl(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10, basis = "log2")
#' }
#'
#' @export

kl <- function(data, id, Group = NULL, opts, choice, epsilon = NULL, basis) {
  if (base::is.null(epsilon)) {
    epsilon <- .00001
  }

  if (!base::is.integer(data[[choice]]) & !base::is.numeric(data[[choice]])) {
    base::stop("Error: Choice must be numeric!")
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])) {
    base::warning("Warning: Grouping variable contains NAs!")
  }

  if (base::anyNA(data[,opts])) {
    base::stop("Error: opts contains NAs!")
  }

  if ((basis != "log") & (basis != "log2")) {
    base::stop("Error: Please specify basis to either 'log' or 'log2'!")
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
      dplyr::mutate(
        pred = base::max.col(WS[,c(base::which(colnames(WS) == "Option_1"):
                                     base::which(colnames(WS) == paste0("Option_", base::length(opts))))]))


    Helper <- tibble::tibble(Options = c(1:base::length(opts)))

    Actual <- tibble::tibble(WS %>%
                               dplyr::group_by(choice) %>%
                               dplyr::summarise(Count = dplyr::n()) %>%
                               dplyr::ungroup() %>%
                               dplyr::mutate(Share = Count / base::sum(Count))
    )

    Predicted <- tibble::tibble(
      WS %>%
      dplyr::group_by(pred) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Predicted = Count / base::sum(Count))
    )

    KL <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

    KL <- base::merge(x = KL, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

    KL[base::is.na(KL)] <- epsilon

    if (basis == "log"){
      Res <- tibble::tibble(cbind(
        KL %>%
          dplyr::reframe(KL_O_P = sum(.[ , 2] * base::log(.[ , 2]/ .[ , 3]))),
        KL %>%
          dplyr::reframe(KL_P_O = sum(.[ , 3] * base::log(.[ , 3]/ .[ , 2])))
      )
      )
    }

    if (basis == "log2"){
      Res <- tibble::tibble(cbind(
        KL %>%
          dplyr::reframe(KL_O_P = sum(.[ , 2] * base::log2(.[ , 2]/ .[ , 3]))),
        KL %>%
          dplyr::reframe(KL_P_O = sum(.[ , 3] * base::log2(.[ , 3]/ .[ , 2])))
      )
      )
    }

    return(Res)
  }

  if (!(base::is.null(Group))) {

    base::colnames(WS) <- c("id", "Group", "choice", paste0("Option_", c(1:base::length(opts))))

    WS <- WS %>%
      dplyr::mutate(
      pred = base::max.col(WS[,c(base::which(colnames(WS) == "Option_1"):
                                   base::which(colnames(WS) == paste0("Option_", base::length(opts))))]))



    KL <- tibble::tibble(Group = base::character(base::length(base::unique(WS$Group)) + 1),
                           KL_O_P = base::numeric(base::length(base::unique(WS$Group)) + 1),
                           KL_P_O = base::numeric(base::length(base::unique(WS$Group)) + 1))

    for (p in 1:base::length(base::unique(WS$Group))) {
      if (p == 1) {
        Helper <- tibble::tibble(Options = c(1:base::length(opts)))

        Actual <- tibble::tibble(
          WS %>%
          dplyr::group_by(choice) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Share = Count / sum(Count))
        )

        Predicted <- tibble::tibble(
          WS %>%
          dplyr::group_by(pred) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Predicted = Count / base::sum(Count))
        )

        DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

        DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

        DataFrame[base::is.na(DataFrame)] <- epsilon

        KL[p, p] <- "All"

        if (basis == "log"){
          KL[p, (p + 1)] <- DataFrame %>%
            dplyr::reframe(sum(.[ , 2] * base::log(.[ , 2]/ .[ , 3])))

          KL[p, (p + 2)] <- DataFrame %>%
            dplyr::reframe(sum(.[ , 3] * base::log(.[ , 3]/ .[ , 2])))
        }


        if (basis == "log2"){
          KL[p, (p + 1)] <- DataFrame %>%
            dplyr::reframe(sum(.[ , 2] * base::log2(.[ , 2]/ .[ , 3])))

          KL[p, (p + 2)] <- DataFrame %>%
            dplyr::reframe(sum(.[ , 3] * base::log2(.[ , 3]/ .[ , 2])))
        }

        base::rm(Helper, Actual, Predicted, DataFrame)

      }

      if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)) {
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))) {
          lab_num <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_num[i])
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
          dplyr::mutate(Share = Count / sum(Count))
      )

      Predicted <- tibble::tibble(
        Group %>%
          dplyr::group_by(pred) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Predicted = Count / base::sum(Count))
      )

      DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

      DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

      DataFrame[base::is.na(DataFrame)] <- epsilon

      KL[(p + 1), 1] <- lab[(p + 1)]

      if (basis == "log"){
        KL[(p + 1), 2] <- DataFrame %>%
          dplyr::reframe(sum(.[ , 2] * base::log(.[ , 2]/ .[ , 3])))

        KL[(p + 1), 3] <- DataFrame %>%
          dplyr::reframe(sum(.[ , 3] * base::log(.[ , 3]/ .[ , 2])))
      }

      if (basis == "log2"){
        KL[(p + 1), 2] <- DataFrame %>%
          dplyr::reframe(sum(.[ , 2] * base::log2(.[ , 2]/ .[ , 3])))

        KL[(p + 1), 3] <- DataFrame %>%
          dplyr::reframe(sum(.[ , 3] * base::log2(.[ , 3]/ .[ , 2])))
      }

      base::rm(Helper, Actual, Predicted, DataFrame)

      if (p == base::max(base::length(base::unique(WS$Group)))) {
        return(KL)
      }
    }
  }
}
