#' Percentage of participants that are reached by assortment
#'
#' @description
#' Reach function of **T**(otal) **U**(nduplicated) **R**(Reach) and **F**(requency) analysis to measure the number of  the averaged percentage of how many participants you can reach
#' (at least one of the products resemble a purchase option) is reached with a specific product bundle assortment.
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"reach"} by group(s)
#' @param opts column names of the alternatives included in the assortment
#' @param none column name of none alternative
#'
#' @details
#' \code{"reach"} calculates the the percentage of consumers that would be reached with the
#' product assortment you are testing. The current logic of \code{reach()}
#' is that the utility of an alternative has to exceed a threshold. In the case of \code{reach()}
#' this threshold is referred to the \code{none} argument in \code{data}.
#'
#'
#' \code{data} has to be a data frame including the alternatives that should be tested
#'
#' \code{group} optional Grouping variable, if results should be displayed by different conditions.
#' Has to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the
#' product assortment that should be considered.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{none} to specify column name of the \code{none} alternative in the
#' validation/holdout task.
#'
#'
#' @importFrom dplyr select mutate across rowwise c_across pick summarise group_by
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
#'   choice = 20, varskeep = 21
#' )
#'
#' # reach ungrouped
#' reach(data = HOT, opts = c(Option_1, Option_2, Option_6), none = None)
#'
#' # reach grouped
#' reach(data = HOT, opts = c(Option_1, Option_2, Option_6), none = None, group = Group)
#' }

#'
#' @export

reach <- function(data, group, none, opts) {
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
    dplyr::select(., {{ opts }}, {{ none }}, {{ group }}) %>% # select relevant variables
    dplyr::mutate(
      thres = {{ none }}, # store utility of threshold
      dplyr::across({{ opts }}, ~ base::ifelse(.x > thres, 1, 0)) # if value of alternatves above threshold --> reached
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(reach = base::ifelse(sum({{ opts }}) > 0, 1, 0)) %>% # if sum of alternatves is at least once above 0 --> reached
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::summarise(reach = base::mean(reach) * 100))
}
