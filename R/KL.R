#' Kullback-Leibler Divergence
#'
#' @description Function to measure the Kullback-Leibler Divergence of a validation/holdout task.
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' to get \code{"kl"} by group(s)
#' @param opts column names of the alternatives included in the
#' validation/holdout task
#' @param choice column name of the actual choice
#' @param epsilon vector of noise that should be added to 0 values, per default set to 1e-05
#' @param base character string to define the logarithm base, currently choice between \code{log} (default) and \code{log2}
#'
#' @return a tibble
#' @importFrom dplyr select mutate group_by pick count summarise
#' @importFrom magrittr "%>%"
#'
#' @details
#' Kullback-Leibler-Divergence which measures the divergence between the actual choice distribution and the predicted
#' choice distribution (Ding et al., 2011; Drost, 2018). Currently only provides
#' the deviation measured based on \eqn{log} and \eqn{log{_2}} algorithm. \eqn{log} set as default.
#'
#' Due to the asymmetry of the Kullback-Leibler divergence, output provides both
#' \code{"KL_O_P"} which is equivalent to (Observed || Predicted) and
#' \code{"KL_P_O"} which is equivalent to (Predicted || Observed).
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
#' \code{epsilon} has to be a numeric input in case of 0 in the numerator or denominator. 0
#' then will be replaced by \code{epsilon}. Default value is \code{epsilon = 1e-5}, however, can
#' be adopted.
#'
#' \code{base} has to be a character string, deciding which logarithm base you want to apply
#' to calculate Kullback-Leibler. You can choose between \eqn{log} and \eqn{log{_2}}. Default set to \eqn{log}.
#'
#' @references {
#'
#' Ding, Min, John R. Hauser, Songting Dong, Daria Dzyabura, Zhilin Yang, SU Chenting, and Steven P. Gaskin. (2011).
#' Unstructured Direct Elicitation of Decision Rules. \emph{Journal of Marketing Research 48}(1): 116-27. \verb{https://doi.org/10.1509/jmkr.48.1.116}.
#'
#' Drost, Hajk-Georg. (2018). Philentropy: Information Theory and Distance Quantification with R. \emph{Journal of Open Source Software 3}(26), 765, \verb{https://joss.theoj.org/papers/10.21105/joss.00765}.
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
#' # kl ungrouped - log
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log"
#' )
#'
#' # kl ungrouped - log2
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log2"
#' )
#'
#' # kl grouped - log + specifying epsilon
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log",
#'   group = Group,
#'   epsilon = 1e-8
#' )
#'
#' # kl grouped - log2
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log2",
#'   group = Group
#' )
#' }
#'
#' @export

kl <- function(data, group, opts, choice, epsilon = NULL, base = NULL) {
  # specify epsilon if not defined
  if (base::is.null(epsilon)) {
    epsilon <- .00001
  }

  if (!base::is.numeric(epsilon)) {
    stop("Error: 'epsilon' has to be numeric!")
  }

  # specify base if not defined
  if (base::is.null(base)) {
    base <- "log"
  }

  if ((base != "log") & (base != "log2")) {
    base::stop("Error: base can only be 'log' or 'log2'!")
  }

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
    dplyr::mutate(alt = base::factor({{ choice }}, levels = c(1:base::length(dplyr::select(., {{ opts }}))), labels = base::paste0("Option_", c(1:base::length(dplyr::select(., {{ opts }})))))) %>% # create factor for actual choice
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count(alt, .drop = F) %>% # count choices
    dplyr::mutate(chosen = n / base::sum(n)) %>% # calculate percentage
    dplyr::select(-"n")) # drop variable

  # create share of predicted choice
  base::suppressMessages(WS2 <- data %>%
    dplyr::mutate(pred = base::max.col(pick({{ opts }}))) %>% # store column index of highest utility
    dplyr::mutate(alt = base::factor(pred, levels = c(1:base::length(dplyr::select(., {{ opts }}))), labels = base::paste0("Option_", c(1:base::length(dplyr::select(., {{ opts }})))))) %>% # create factor
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count(alt, .drop = F) %>% # count number of predicted choice
    dplyr::mutate(pred = n / base::sum(n)) %>% # calculate percentage
    dplyr::select(-"n")) # drop variable

  # if base set to 'log'
  if (base == "log") {
    return(WS1 %>%
      base::merge(x = ., y = WS2, by = c(WS1 %>% dplyr::select(., {{ group }}) %>% base::colnames(), "alt")) %>% # merge both data frames
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::mutate(
        chosen = base::ifelse(chosen == 0, epsilon, chosen), # add epsilon if 0
        pred = base::ifelse(pred == 0, epsilon, pred) # add epsilon if 0
      ) %>%
      dplyr::summarise(
        kl_o_p = base::sum(chosen * base::log(chosen / pred)),
        kl_p_o = base::sum(pred * base::log(pred / chosen))
      ))
  }

  # if base set to 'log2'
  if (base == "log2") {
    return(WS1 %>%
      base::merge(x = ., y = WS2, by = c(WS1 %>% dplyr::select(., {{ group }}) %>% base::colnames(), "alt")) %>% # merge both data frames
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::mutate(
        chosen = base::ifelse(chosen == 0, epsilon, chosen), # add epsilon if 0
        pred = base::ifelse(pred == 0, epsilon, pred) # add epsilon if 0
      ) %>%
      dplyr::summarise(
        kl_o_p = base::sum(chosen * base::log2(chosen / pred)),
        kl_p_o = base::sum(pred * base::log2(pred / chosen))
      ))
  }
}
