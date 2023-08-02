#' MHP
#'
#' @description
#' Function to measure the mean hit probability of a validation/
#' holdout task. Calculates the hit/ choice probability of the actual choice
#' in the validation/ holdout task.
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"mhp"} by group(s)
#' @param opts column names of the alternatives included in the
#' validation/holdout task
#' @param choice column name of the actual choice
#'
#' @details
#' Mean hit probability (MHP) measures the averaged hit probability of participants actual
#' choices in the validation/ holdout task.
#'
#' \code{data} needs to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()} function.
#'
#' \code{group} optional Grouping variable, if results should be display by different groups.
#' Needs to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the validation/holdout
#' task (also includes the \code{none} alternative).
#' Input of \code{opts} needs to be column names of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice.
#' Input of opts \code{choice} needs to be column name of actual choice.
#'
#' @return a tibble
#' @importFrom dplyr select relocate mutate rowwise pick across ungroup group_by summarise
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
#'   choice = 20, varskeep = 21
#' )
#'
#' # mhp ungrouped
#' mhp(data = HOT, opts = c(Option_1:None), choice = choice)
#'
#' # mhp grouped
#' mhp(data = HOT, opts = c(Option_1:None), choice = choice, group = Group)
#'
#' }
#'
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

  rm(choi)

  #############################################################################
  # change data structure
  data <- data %>%
    dplyr::relocate(., c({{ opts }}, {{ choice }}, {{ group }})) %>%
    dplyr::mutate(dplyr::across({{ opts }}, base::exp)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Summe = base::sum(dplyr::pick({{ opts }}))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>%
    dplyr::mutate(mhp = 0)


  choi <- data %>%
    dplyr::select(., {{ choice }}) %>%
    base::unlist() %>%
    base::unname()


  for (j in 1:nrow(data)) {
    data$mhp[j] <- base::unlist(data[j, choi[j]])
  }


  return(suppressMessages(data %>%
                            dplyr::group_by(dplyr::pick({{ group }})) %>%
                            dplyr::summarise(MHP = base::mean(mhp))))
}
