#' Median Absolute Error
#'
#' @description
#' Function to measure the median absolute error (MedAE) of a validation/holdout
#' task.
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"medae"} by group(s)
#' @param opts column names of the alternatives included in the
#' validation/holdout task
#' @param choice column name of the actual choice
#'
#' @return a tibble
#' @importFrom dplyr select mutate group_by pick count ungroup across summarise
#' @importFrom magrittr "%>%"
#' @importFrom stats median
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect ends_with
#'
#' @details
#' Median absolute error (MedAE) calculates the deviation between predicted and
#' stated (actual) choice share and returns the median error in the
#' validation/holdout task, which is less likely to be influenced by outliers.
#'
#' \code{data} has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()}
#' function.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups.Has to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the
#' validation/holdout task. Input of \code{opts} has to be column names
#' of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice.
#' Input of opts \code{choice} has to be column name of actual choice.
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
#'   choice = 20,
#'   varskeep = 21
#' )
#'
#' # medae ungrouped
#' medae(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice
#' )
#'
#' # medae grouped
#' medae(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   group = Group
#' )
#' }
#'
#' @export


medae <- function(data, group, opts, choice) {
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

  # create actual share of actual choice
  base::suppressMessages(WS1 <- data %>%
    dplyr::mutate(
      # create factor
      merger = base::factor(
        {{ choice }},
        levels = c(1:base::length(dplyr::select(., {{ opts }}))),
        labels = c(1:base::length(dplyr::select(., {{ opts }}))))) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count(merger, .drop = F) %>% # count choices
    dplyr::mutate(chosen = n / base::sum(n) * 100) %>% # calculate percentage
    dplyr::select(-"n")) # drop variable

  # create predicted share
  base::suppressMessages(WS2 <- data %>%
                           # exponentiate
    dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>%
    dplyr::rowwise() %>%
      # calculate sum rowwise
    dplyr::mutate(Summe = base::sum(dplyr::pick({{ opts }}))) %>%
    dplyr::ungroup() %>%
      # calculate choice probability
    dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
      # aggregate choice probability
    dplyr::summarise(across({{ opts }}, ~ mean(.x),
                            .names = "{.col}_mean")) %>%
      # change to longer format
    tidyr::pivot_longer(.,
                        cols = tidyselect::ends_with("_mean"),
                        names_to = "alt", values_to = "mean") %>%
    dplyr::mutate(
      # adapt labeling
      alt = base::substr(alt, 1,
                         (base::nchar(alt) - base::nchar("_mean"))),
      merger = base::rep(1:base::length(dplyr::select(data, {{ opts }})),
                         length.out = base::length(alt)) # create merger
    ))

  return(suppressMessages(WS2 %>%
    base::merge(x = .,
                y = WS1,
                by = c(WS2 %>% dplyr::select(., {{ group }}) %>%
                         base::colnames(), "merger")) %>% # merge
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::mutate(MEDAE = base::abs(mean - chosen)) %>% # calculate medae
    dplyr::summarise(medae = stats::median(MEDAE))))
}
