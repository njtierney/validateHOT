#' Accuracy
#'
#' @description accuracy is one of the 5 metrics of the confusion matrix
#' and is defined as number of correct predicted participants divided by the total number of predictions.
#' See, for example, Burger (2018): \eqn{\frac{TP + TN}{TP + FP + TN + FN}}, where TP =
#' True Positives, TN = True Negatives, FP = False Positives, and FN =
#' False Negatives.
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"accuracy"} by group(s)
#' @param opts column names of the alternatives included in the
#' validation/holdout task
#' @param choice column name of the actual choice
#' @param none column name of none alternative
#'
#' @details
#' The current logic of \code{"accuracy"} is to provide whether a binary coded event is correctly predicted.
#' To use the function a \code{"none"} alternative needs to be in the script.
#' One potential usage is, for example, whether a buy or a no-buy condition
#' was predicted correctly. For example, you have three alternatives plus
#' a \code{"none"} alternative and you want to check whether a buy or no-buy was
#' correctly predicted. This function can be helpful when you test whether or
#' not your model significantly overestimates or underestimates, for example, a purchase likelihood.
#' This function was programmed to predict demand for a MaxDiff, CBC, or ACBC.
#'
#'
#' \code{data} needs to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()} function.
#'
#' \code{group} optional grouping variable(s), if results should be display by different groups.
#' Need to be column name(s) of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the validation/holdout
#' task (also includes the \code{none} alternative).
#' Input of \code{opts} needs to be column names of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice.
#' Input of \code{choice} needs to be column name of actual choice.
#'
#' \code{none} to specify column name of the \code{none} alternative in the
#' validation/holdout task.
#'
#' @importFrom dplyr group_by summarise select pick
#' @importFrom magrittr "%>%"
#'
#' @return a tibble
#'
#' @seealso {
#' \code{\link[=f1]{f1}}
#' \code{\link[=precision]{precision}}
#' \code{\link[=recall]{recall}}
#' \code{\link[=specificity]{specificity}}
#' }
#'
#' @references {
#'
#' Burger, S. V. (2018). \emph{Introduction to Machine Learning with R: Rigorous Mathematical Analysis}. O'Reilly.
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
#'   varskeep = 21,
#'   choice = 20
#' )
#' # accuracy ungrouped
#' accuracy(data = HOT, opts = c(Option_1:None), choice = choice, none = None)
#'
#' # accuracy by group
#' accuracy(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = Group)
#' }
#' @export
accuracy <- function(data, group, opts, choice, none) {
  # check for wrong / missing input
  if (base::length(data %>% dplyr::select(., {{ none }})) == 0) {
    stop("Error: argument 'none' is missing!")
  }

  # check whether opts are defined
  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 1) {
    stop("Error: specify at least 2 alternatives in 'opts'!")
  }

  # check whether none is part of opts
  if (!(data %>% dplyr::select(., {{ none }}) %>% base::colnames()) %in%
    (data %>% dplyr::select(., {{ opts }}) %>% base::colnames())) {
    stop("Error: 'none' needs to be part of 'opts'!")
  }

  # grouping variable
  ## check for missings
  if (base::anyNA(data %>% dplyr::select(., {{ group }}))) {
    warning("Warning: 'group' contains NAs!")
  }

  # alternatives
  ## store names of alternatives
  alternatives <- data %>%
    dplyr::select(., {{ opts }}) %>%
    base::colnames()

  ## check whether variable is numeric
  for (i in 1:base::length(alternatives)) {
    if (!base::is.numeric(data[[alternatives[i]]])) {
      stop("Error: 'opts' need to be numeric!")
    }
  }

  ## check for missings
  if (anyNA(data %>% dplyr::select(., {{ opts }}))) {
    stop("Error: 'opts' contains NAs!")
  }

  # choice
  ## check for missing
  if (base::anyNA(data %>% dplyr::select(., {{ choice }}))) {
    stop("Error: 'choice' contains NAs!")
  }

  ## check for str
  choi <- data %>%
    dplyr::select(., {{ choice }}) %>%
    base::colnames()

  if (!base::is.numeric(data[[choi]])) {
    stop("Error: 'choice' needs to be numeric!")
  }

  return(data %>%
    dplyr::mutate(
      pred = base::max.col(dplyr::pick({{ opts }})),
      buy = base::ifelse({{ choice }} != base::match(
        data %>% dplyr::select(., {{ none }}) %>% colnames(),
        data %>% dplyr::select(., {{ opts }}) %>% colnames()
      ), 1, 2),
      pred = base::ifelse(pred != base::match(
        data %>% dplyr::select(., {{ none }}) %>% colnames(),
        data %>% dplyr::select(., {{ opts }}) %>% colnames()
      ), 1, 2)
    ) %>%
    dplyr::group_by(pick({{ group }})) %>%
    dplyr::summarise(
      accuracy = 100 * base::mean(buy == pred)
    ))
}
