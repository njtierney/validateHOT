#' Probability Scores for (anchored) MaxDiff
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' @param items specifies the items
#' @param set.size size of the choice sets
#' @param anchor optional variable to specify anchor variable
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#'
#' prob_scores(data = MaxDiff,
#'             items = c(Option_01:Option_16),
#'             set.size = 4,
#'             anchor = "none"
#')
#'
#' prob_scores(data = MaxDiff,
#'             items = c(Option_01:Option_16),
#'             set.size = 4,
#'             anchor = "none"
#')
#' }
#' @export
prob_scores <- function(data, group = NULL, items, set.size, anchor = NULL){




  if (base::is.null(anchor)){
    data %>%
      dplyr::mutate(dplyr::across({{ items }}, ~ (base::exp(.x) / (base::exp(.x) + (set.size - 1))))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Summe = base::sum(dplyr::pick({{ items }}))) %>% # sum up
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across({{ items }}, ~ .x / Summe * 100)) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::summarise(dplyr::across({{ items }},
                                     c(mw = base::mean, std = stats::sd),
                                     .names = "{.col}.{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
                          cols = tidyselect::ends_with(c(".mw", ".std")),
                          names_to = c("Option", ".value"), names_sep = "\\."
      )
  }

  if (!base::is.null(anchor)){
    return(
      data %>%
        dplyr::mutate(across(c({{ items }}, anchor), ~ (base::exp(.x) / (base::exp(.x) + (set.size - 1))) * 100 / (1 / set.size))) %>%
        dplyr::group_by(dplyr::pick({{ group }})) %>%
        dplyr::summarise(dplyr::across(c({{ items }}, anchor),
                                       c(mw = base::mean, std = stats::sd),
                                       .names = "{.col}.{.fn}"
        )) %>%
        tidyr::pivot_longer(.,
                            cols = tidyselect::ends_with(c(".mw", ".std")),
                            names_to = c("Option", ".value"), names_sep = "\\."
        ))
  }


}
