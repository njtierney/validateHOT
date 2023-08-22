#' Hit Rate
#'
#' @description \code{hitrate} measures number of times a choice was correctly predicted in a validation/holdout task.
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"hitrate"} by group(s)
#' @param opts column names of the alternatives included in the
#' validation/holdout task
#' @param choice column name of the actual choice
#'
#' @details
#' \code{hitrate} measures number of times a participant's choice was correctly
#' predicted by the model.
#' Output contains the following 4 metrics:
#' \itemize{
#' \item \code{"chance"} chance level (\eqn{\frac{1}{number of alternatives}}) in percentage
#' \item \code{"no."} absolute value of correctly predicted choices
#' \item \code{"\%"} correctly predicted choices in percentage
#' \item \code{"n"} total number of choices
#' }
#'
#' \code{data} has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()} function.
#'
#' \code{group} optional grouping variable, if results should be displayed by different groups.
#' Has to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the validation/holdout
#' task (also includes the \code{none} alternative).
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice.
#' Input of opts \code{choice} has to be column name of actual choice.
#'
#' @return a tibble
#' @importFrom dplyr select mutate pick group_by summarise n
#' @importFrom magrittr "%>%"
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
#'   choice = 20,
#'   varskeep = 21
#' )
#' # hit rate ungrouped
#' hitrate(data = HOT, opts = c(Option_1:None), choice = choice)
#'
#' # hit rate grouped
#' hitrate(data = HOT, opts = c(Option_1:None), choice = choice, group = Group)
#' }
#'
#' @export

hitrate <- function(data, group, opts, choice) {
  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 1) {
    stop("Error: specify at least 2 alternatives in 'opts'!")
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
      stop("Error: 'opts' has to be numeric!")
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
    stop("Error: 'choice' has to be numeric!")
  }


  suppressMessages(return(data %>%
    dplyr::mutate(pred = base::max.col(dplyr::pick({{ opts }}))) %>% # store column index with highest utility
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::summarise(
      HR = mean(as.integer({{ choice }} == pred)) * 100, # calculate the hit rate
      chance = 1 / base::length(dplyr::select(data, {{ opts }})) * 100, # calculate the chance level
      cor = base::sum(base::as.integer({{ choice }} == pred)), # calculate number of correct predicted
      n = dplyr::n() # n
    )))
}
