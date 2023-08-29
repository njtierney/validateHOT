#' Averaged number of times a person is reached by a specific assortment of bundles
#'
#' @description
#' Frequency function of T(otal) U(nduplicated) R(each) and F(requency)
#' analysis to measure the average time a consumer
#' is reached with a specific product bundle assortment. \code{"freqassort"} calculates
#' the frequency based on the 'threshold' approach, meaning each alternative that exceeds
#' utility of \code{none} alternative is considered as, for example, purchase option.
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"freqassort"} by group(s)
#' @param opts column names of the alternatives included in the assortment
#' @param none column name of none / threshold alternative
#'
#' @details
#' Frequency calculates the average times a consumer would be reached with the
#' product assortment you are testing. The current logic of \code{freqassort()}
#' is that the utility of an alternative has to exceed a threshold. In the case of \code{freqassort()}
#' this threshold is referred to the \code{none} argument in \code{data}.
#'
#' \code{data} has to be a data frame including the alternatives that should be tested
#'
#' \code{group} optional grouping variable, if results should be displayed by different conditions.
#' Needs to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the
#' product assortment that should be considered.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{none} to specify column name of the \code{none} alternative in the
#' validation/holdout task.
#'
#'
#' @importFrom dplyr select mutate across rowwise c_across pick summarise group_by ungroup
#' @importFrom magrittr "%>%"
#'
#' @return a tibble
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
#' # freqassort ungrouped
#' freqassort(data = HOT,
#'            opts = c(Option_1, Option_2, Option_6),
#'            none = None)
#'
#' # freqassort grouped
#' freqassort(data = HOT,
#'            opts = c(Option_1, Option_2, Option_6),
#'            none = None,
#'            group = Group)
#' }
#'
#' @export
freqassort <- function(data, group, none, opts) {
  # check for wrong / missing input
  if (base::length(data %>% dplyr::select(., {{ none }})) == 0) {
    stop("Error: argument 'none' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }

  # grouping variable
  ## store names of grouping variables
  groups <- data %>%
    dplyr::select(., {{ group }}) %>%
    base::colnames()

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

  # None
  ## check for missing
  if (base::anyNA(data %>% dplyr::select(., {{ none }}))) {
    stop("Error: 'none' contains NAs!")
  }

  ## check for str
  Noo <- data %>%
    dplyr::select(., {{ none }}) %>%
    base::colnames()

  if (!base::is.numeric(data[[Noo]])) {
    stop("Error: 'none' needs to be numeric!")
  }

  ## check none can not be part of opts
  if ((data %>% dplyr::select(., {{ none }}) %>% base::colnames()) %in%
    (data %>% dplyr::select(., {{ opts }}) %>% base::colnames())) {
    stop("Error: 'none' can not be part of 'opts'!")
  }

  return(data %>%
    dplyr::select(., {{ opts }}, {{ none }}, {{ group }}) %>%
    dplyr::mutate(
      thres = {{ none }}, # store threshold utility
      dplyr::across({{ opts }}, ~ base::ifelse(.x > thres, 1, 0))
    ) %>% # recode opts depending whether it is higher (1) or lower (0) than the threshold
    dplyr::rowwise() %>%
    dplyr::mutate(freq = base::sum(dplyr::c_across({{ opts }}))) %>% # sum the number of options rowwise
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::summarise(freq = base::mean(freq)))
}
