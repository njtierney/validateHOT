#' Function to calculate Mean Hit Probability (MHP) of validation task
#'
#' @description
#' \code{mhp} measures the mean hit probability of a validation/holdout
#' task. Calculates the hit/choice probability of the actual choice
#' in the validation/holdout task.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get \code{mhp} by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice.
#'
#' @details
#' Mean hit probability (MHP) measures the averaged hit probability of
#' participants actual choices in the validation/holdout task.
#'
#' \code{data} has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()}
#' function.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the
#' validation/holdout task.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice.
#' Input of opts \code{choice} has to be column name of actual choice.
#'
#' Output will display both mean hit probability and its corresponding standard
#' error.
#'
#' @return a tibble
#' @importFrom dplyr select relocate mutate rowwise pick across ungroup
#' group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
#' HOT <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   none = 19,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20,
#'   varskeep = 21
#' )
#'
#' # mhp ungrouped
#' mhp(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice
#' )
#'
#' # mhp grouped
#' mhp(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   group = Group
#' )
#' }
#'
#' @export
mhp <- function(data, group, opts, choice) {
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

  rm(choi)

  #############################################################################
  # change data structure
  data <- data %>%
    # reorder columns
    dplyr::relocate(., c({{ opts }}, {{ choice }}, {{ group }})) %>%
    # exponentiate
    dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>%
    dplyr::rowwise() %>%
    # calculate sum
    dplyr::mutate(Summe = base::sum(dplyr::pick({{ opts }}))) %>%
    dplyr::ungroup() %>%
    # choice probability in percentage
    dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>%
    # create mhp variable
    dplyr::mutate(mhp = 0)


  # store actual choice
  choi <- data %>%
    dplyr::select(., {{ choice }}) %>%
    base::unlist() %>%
    base::unname()

  # assign correct hit probability
  for (j in 1:nrow(data)) {
    data$mhp[j] <- base::unlist(data[j, choi[j]])
  }

  # calculate MHP
  return(suppressMessages(data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::summarise(
      MHP = base::mean(mhp),
      se = (stats::sd(mhp) / base::sqrt(dplyr::n()))
    )))
}
