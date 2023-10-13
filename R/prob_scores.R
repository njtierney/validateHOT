#' Title
#'
#' @param data a
#' @param group v
#' @param items v
#' @param set.size c
#' @param anchor c
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
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
