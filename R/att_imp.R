#' Title
#'
#' @param data data frame with all relevant variables
#' @param group optional column name(s) to specify grouping variable(s)
#' @param attrib specifies the attribute levels for each attribute
#' @param coding a vector of the coding of each attribute, '0' = part-worth
#' coding, '1' = linear coding
#' @param interpolate.levels a list of the attribute levels that should
#' be interpolated. These have to be the same as provided to Sawtooth Software.
#' Please make sure to provide the whole list. Only has to be specified for the
#' variables that are coded as '1' (linear)
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' att_imp(data = CBC,
#' attrib = list(c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
#' c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
#' c("Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5", "Att3_Lev6", "Att3_Lev7")),
#' coding = c(0, 0, 0))
#'
#' att_imp(data = CBC_lin,
#' attrib = list(c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
#' c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
#' c("Att3_Lin")),
#' coding = c(0, 0, 1),
#' interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)))
#'
#' }
#'
#' @export
att_imp <- function(data, group = NULL, attrib, coding, interpolate.levels = NULL){

  att <- length(attrib)

  new <- c()

  for (i in 1:att){

    helper <- 1

    data[[paste0("att_imp", i)]] <- 0

    vars <- attrib[[i]]

    new <- c(new, paste0("att_imp", i))

    for (j in 1:nrow(data)){

      if (coding[i] == 0){

        data[j, paste0("att_imp", i)] <- abs(diff(range(data[j, vars])))

      }

      if (coding[i] == 1) {


        data[j, paste0("att_imp", i)] <- abs(data[j, vars] * abs(diff(range(interpolate.levels[[helper]]))))

        helper

      }

    }

    if (coding[i] == 1){
      helper <- helper + 1
    }

  }

  return(data %>%
           mutate(across(all_of(new), ~ .x / rowSums(data[new]))) %>%
           dplyr::group_by(dplyr::pick({{ group }})) %>%
           summarise(across(all_of(new), c(mw = base::mean, std = stats::sd),
                            .names = "{.col}.{.fn}"
           )) %>%
           tidyr::pivot_longer(.,
                               cols = tidyselect::ends_with(c(".mw", ".std")),
                               names_to = c("Option", ".value"), names_sep = "\\."
           ) %>%
           mutate_at(vars(mw, std), ~ .x * 100)
  )
}
