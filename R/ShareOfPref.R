#' Share of Preferences of Options included in HOT
#'
#' @description
#' Function to measure the share of preferences of each option in the validation task
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"shareofpref"} by group(s)
#' @param opts column names of the alternatives included in the
#' validation/holdout task
#'
#' @return a tibble
#' @importFrom dplyr select pick mutate across ungroup group_by summarise count
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom tibble as_tibble
#'
#' @details
#' Share of Preference provides the aggrgated share of each alternative in the
#' validation/ holdout task as well as the lower and upper confidence interval
#' of each alternative which is calculated according to the following formula
#' \eqn{mean +/- 1.96 x \frac{sd}{\sqrt(n)}} (Orme, 2020, p. 94).
#'
#' \code{data} needs to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()} function.
#'
#' \code{group} optional Grouping variable, if results should be display by different groups.
#' Needs to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the simulation
#' task (also includes the \code{none} alternative).
#' Input of \code{opts} needs to be column names of variables in \code{data}.
#'
#' @references {
#'
#' Orme, B. K. (2020). \emph{Getting Started with Conjoint Analysis:
#' Strategies for Product Design and Pricing Research}. 4th edition.
#' Manhattan Beach, CA: Research Publishers LLC.
#'
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
#'   choice = 20,
#'   varskeep = 21
#' )
#'
#' # shareofpref ungrouped
#' shareofpref(data = HOT, opts = c(Option_1:None))
#'
#' # shareofpref grouped
#' shareofpref(data = HOT, group = Group, opts = c(Option_1:None))
#' }
#'
#' @export

shareofpref <- function(data, group, opts) {

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


  WS1 <- data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count()

  # change data structure
  return(data %>%
           dplyr::mutate(dplyr::across({{ opts }}, base::exp)) %>%
           dplyr::rowwise() %>%
           dplyr::mutate(Summe = base::sum(dplyr::pick({{ opts }}))) %>%
           dplyr::ungroup() %>%
           dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>%
           dplyr::group_by(dplyr::pick({{ group }})) %>%
           dplyr::summarise(across({{opts}}, c(mw = base::mean, std = stats::sd), .names = "{.col}.{.fn}")) %>%
           tidyr::pivot_longer(., cols = tidyselect::ends_with(c(".mw", ".std")) ,
                               names_to = c("Option", ".value"), names_sep = "\\.") %>%
           base::merge(x = ., y = WS1, by =c(data %>% dplyr::select(., {{group}}) %>% base::colnames())) %>%
           dplyr::mutate(se = std/sqrt(n),
                         lo.ci = mw - (1.96 * se),
                         up.ci = mw + (1.96 * se)) %>%
           dplyr::select(!tidyselect::all_of(c("std", "n")))%>%
           tibble::as_tibble())

}
