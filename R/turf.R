#' Total Unduplicated Reach and Frequency
#' @description
#' T(otal) U(nduplicated) R(each) and F(requency) is
#' a "product line extension model" (Miaoulis et al., 1990, p. 29). For each possible combinations,
#' it looks for the reach and frequency of this combination. Participants are reached,
#' if at least one of the alternatives in a combination has a higher utility than \code{none}. On the contrary,
#' frequency calculates the averaged number of alternatives that have a higher utility than \code{none}.
#'
#'
#' @param data data frame with all relevant variables
#' @param opts column names of the alternatives included in the assortment
#' @param none column name of none / threshold alternative
#' @param size numeric vector to determine size of the assortment
#' @param fixed optional vector to determine alternatives that have to be included in the assortment
#' @param approach character whether to run First Choice approach ('fc') or Threshold approach ('thres')
#'
#' \code{data} has to be a data frame including the alternatives that should be tested
#'
#' \code{opts} is needed to specify the different alternatives in the
#' product assortment that should be considered.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{none} to specify column name of the \code{none} alternative in the
#' validation/holdout task.
#'
#' \code{size} has to be a whole number determining the size of the assortment.
#'
#' \code{fixed} has to be a vector of variables that are fixed in the assortment, i.e., they
#' have to be part of the assortment
#'
#' \code{approach} character defining whether first choice \code{approach = 'fc'} or
#' threshold \code{approach = 'thres'}. If \code{approach = 'fc'}, participants are
#' considered being reached, if there alternative with highest utility is included in the assortment (Chrzan & Orme, 2019, p. 111).
#' On the contrary, if \code{approach = 'thres'}, participants are considered being reached, if utility of one product is
#' highr than the one of the \code{none} alternative (Chrzan & Orme, 2019, p. 112). If \code{approach = 'fc'}, \code{reach} equals \code{freq} since participants
#' have at maximum their most preferred alternative that exceeds the \code{none} alternative.
#'
#' @references {
#'
#' Chrzan, K., & Orme, B. K. (2019). \emph{Applied MaxDiff: A Practitioner’s Guide to Best-Worst Scaling} Provo, UT: Sawtooth Software.
#'
#' Miaoulis, G., Parsons, H., & Free, V. (1990). Turf: A New Planning Approach for Product Line Extensions. \emph{Marketing Research 2} (1): 28-40.
#'
#' }
#'
#' @importFrom dplyr across arrange filter mutate rename rename_all relocate select
#' @importFrom magrittr "%>%"
#' @importFrom utils combn
#' @importFrom tidyselect all_of everything starts_with
#' @importFrom tibble remove_rownames
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' HOT <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   None = 19,
#'   prod = 7,
#'   prod.levels = list(3, 4, 5, 6, 7, 8, 9, 10, 11,
#'                      12, 13, 14, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20
#' )
#'
#' # turf no fixed alternatives
#' t1 <- turf(data = HOT,
#'            opts = c(Option_1:Option_16),
#'            none = None,
#'            size = 3,
#'            approach = "thres")
#'
#' head(t1)
#'
#' # turf alternative 4 and 5 fixed
#' t2 <- turf(data = HOT,
#'            opts = c(Option_1:Option_16),
#'            none = None,
#'            size = 4,
#'            fixed = c("Option_4", "Option_5"),
#'            approach = "thres")
#'
#' head(t2)
#' }
#'
#'@export
turf <- function(data, opts, none, size, fixed = NULL, approach = c("thres", "fc")) {

  # check for wrong / missing input
  if (base::length(data %>% dplyr::select(., {{ none }})) == 0) {
    base::stop("Error: argument 'none' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    base::stop("Error: argument 'opts' is missing!")
  }

  # size can not be larger than or equal to opts
  if (size > base::length(data %>% dplyr::select(., {{ opts }}))){
    base::stop("Error: 'size' can not be larger than size of 'opts'!")
  }

  # approach needs to be specified
  if (base::missing(approach)){
    base::stop("Error: 'approach' is missing!")
  }

  # approach can only be 'thres' or 'fc'
  if ((approach != "thres") & (approach != "fc")) {
    base::stop("Error: 'approach' is wrong, please choose between 'fc' and 'thres'!")
  }

  # approach is missing
  if (base::missing(approach)) {
    base::stop("Error: 'approach' is not defined!")
  }

  # size has to be numeric
  ## imports whole number function from base package
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if (!is.wholenumber(size)){
    base::stop("Error: 'size' must be a whole number!")
  }
  rm(is.wholenumber)

  # fixed has to be part of opts
  if (!base::is.null(fixed)){
    fixed <- data %>% dplyr::select(tidyselect::all_of(fixed)) %>% base::colnames()

    if (!base::all(fixed %in% (data %>% dplyr::select(., {{ opts }}) %>% base::colnames()))){
      base::stop("Error: 'fixed' has to be part of 'opts'!")
    }
  }

  # fixed can not be larger than size
  if (!base::is.null(fixed)){
    if (length(data %>% dplyr::select(tidyselect::all_of(fixed))) > size){
      stop("Error: 'fixed' can not be larger than 'size'!")
    }
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
    stop("Error: 'none' has to be numeric!")
  }

  ## check none can not be part of opts
  if ((data %>% dplyr::select(., {{ none }}) %>% base::colnames()) %in%
      (data %>% dplyr::select(., {{ opts }}) %>% base::colnames())) {
    stop("Error: 'none' can not be part of 'opts'!")
  }


  # prepare data frame and threshold approach
  if (approach == "thres") { # threshold approach
    df <- data %>%
      dplyr::select(., {{ opts }}, {{ none }}) %>% # select relevant variables
      dplyr::rename("thres" = ncol(.)) %>% # rename variable
      dplyr::mutate(
        dplyr::across({{ opts }}, ~ base::ifelse(.x > thres, 1, 0)) # if value of alternatives above threshold --> reached
      ) %>%
      dplyr::select(-thres) # delete threshold
  }


  if (approach == "fc") { # first choice rule
    df <- data %>%
      dplyr::select(., {{ opts }}, {{ none }}) %>% # select relevant variables
      dplyr::rename("thres" = ncol(.)) %>% # rename variable
      dplyr::mutate(maximum = base::max.col(.)) # store column index with highest value

    for (row in 1:base::nrow(df)) { # loop for each row
      for (col in 1:(base::ncol(df) - 2)) { # loop for each column (except for last two --> thres and maximum)
        if (df[row, col] > df[row, "thres"] & col == df[row, "maximum"]) { # only run if larger than threshold and if row maximum
          df[row, col] <- 1 # assign buy
        } else {
          df[row, col] <- 0 # assign no buy
        }
      }
    }

    df <- df %>%
      dplyr::select(-c(maximum, thres)) # delete irrelevant rows
  }

  # prepare items
  items <- data %>%
    dplyr::select(., {{ opts }}) %>% # select specified opts
    base::colnames() # store column names only

  # define new variable names
  var_names <- c(items, paste0("new_col_names_", c(1:size)))
  var_names <- base::make.unique(var_names, sep = "...")
  var_names <- var_names[-c(1:length(items))]

  # create combos
  combos <- base::as.data.frame(base::t(utils::combn(items, size))) %>% # create all possible combinations
    dplyr::rename_all(., ~ var_names) # rename variables

  if (!base::is.null(fixed)) { # only run if there are fixed values and delete ones that do not contain fixed values
    # create all possible combinations

    fixies <- data %>%
      dplyr::select(., {{ fixed }}) %>% # store the fixed values
      colnames() # store the column names

    combos <- combos %>%
      dplyr::mutate(must = base::apply(., 1, function(x) base::as.integer(base::all(fixies %in% x)))) %>% # check for each combo whether the fixed ones are included
      dplyr::filter(must == 1) %>% # only choose those that have the fixed options
      dplyr::select(-must) # delete must variable
  }

  # count reach and frequency for each
  for (i in 1:nrow(combos)) {
    combs <- base::unname(c(base::unlist(combos[i, ]))) # store the combos as vector

    df[[base::paste0("comb.", base::paste0(combs, collapse = "_"))]] <- base::rowSums(df[, combs]) # create combination and store the rowsum for that combination for each participant
  }


  # next create the total data frame
  total <- base::merge(
    x = (df %>%
           dplyr::select(tidyselect::all_of(tidyselect::starts_with("comb."))) %>% # only select the variables that start with comb. (see approach before)
           dplyr::summarise(dplyr::across(1:base::ncol(.), ~ (base::sum(. > 0) / base::nrow(df) * 100))) %>% # create the reach score
           base::t() %>% # transpose
           base::as.data.frame() %>% # store as data frame
           dplyr::rename_all(., ~"reach") %>% # rename variable into 'reach'
           dplyr::mutate(combo = base::rownames(.)) %>% # store rownames in new variable
           tibble::remove_rownames() %>% # remove rownames
           dplyr::relocate(combo, .before = tidyselect::everything())), # relocate combo variable at the beginning
    y = (df %>%
           dplyr::select(tidyselect::all_of(tidyselect::starts_with("comb."))) %>% # only select the variables that start with comb. (see approach before)
           dplyr::summarise(dplyr::across(1:base::ncol(.), ~ base::mean(.x))) %>% #create frequency score
           base::t() %>% # transpose
           base::as.data.frame() %>% # store as data frame
           dplyr::rename_all(., ~"freq") %>% # rename variable into 'freq'
           dplyr::mutate(combo = base::rownames(.)) %>% # store rownames in new variable
           tibble::remove_rownames() %>% # remove rownames
           dplyr::relocate(combo, .before = tidyselect::everything())), # relocate combo variable at the beginning
    by = "combo" # merge both by 'combo'
  )

  # prepare a new data frame that mimics output of turfR (variables are coded as 1 and 0 whether or not item is present in that assortment or not)
  new_df <- base::data.frame(base::matrix(nrow = base::nrow(combos), ncol = base::length(items))) %>% # create a new data frame
    dplyr::rename_all(., ~ items) %>% # rename columns
    dplyr::mutate_all(., ~ base::ifelse(base::is.na(.x), 0, NA)) %>%  # replace nas by 0s
    base::cbind(combos, .) # bind with combos

  # prepare the binary coding
  for (i in 1:base::nrow(new_df)) { # loop for each row
    for (j in 1:size) { # repeat for each assortment
      new_df[i, new_df[i, j]] <- 1 # store 1
    }
  }

  # create 'combo' variable for merging purposes
  new_df <- new_df %>%
    dplyr::mutate(combo = 0)

  for (i in 1:base::nrow(new_df)) { # loop for merging purposes
    new_df[i, "combo"] <- base::paste0("comb.", base::paste0(new_df[i, c(1:size)], collapse = "_")) # store the names in the variable for merging purposes
  }

  # prepare final step

  return(new_df %>%
         dplyr::select(-tidyselect::all_of(base::colnames(new_df)[1:size])) %>% # delete the first variables (variables indicating name of item)
         base::merge(x = total, y = ., by = "combo") %>% # merge with total
         arrange(-reach, -freq) %>% # arrange  # sort descending for reach and frequency
         mutate(combo = paste0("Combo ", dplyr::row_number()))) # rename combo
}
