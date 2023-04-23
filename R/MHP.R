#' MHP
#'
#' @description Function to measure the mean hit probability of a holdout task
#' @param data a data frame
#' @param id column index of the \code{id} variable
#' @param Group optional grouping variable to get hit rate by group
#' @param opts column indexes of the options included in the holdout task
#' @param choice column index of the actual choice
#'
#' @return xyz
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff")
#' MHP(data = HOT, id = 1, opts = c(2:9), choice = 10)
#'
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff", varskeep = 21)
#' MHP(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)
#'
#'
#' @export
MHP <- function(data, id, Group = NULL, opts, choice) {

  if (!base::is.integer(data[[choice]]) & !base::is.numeric(data[[choice]])){
    base::stop("Error: Choice must be numeric!")
  }

  WS <- data[, c(id, Group, choice, opts)]

  if (base::is.null(Group)) {
    Options <- c()

    for (k in 1:base::length(opts)) {
      name <- base::paste0("Option_", k)
      Options <- c(Options, name)
    }

    newNames <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Opt_", k)
      newNames <- c(newNames, name)
    }

    Perc <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Perc_", k)
      Perc <- c(Perc, name)
    }

    base::colnames(WS) <- c("id", "choice", Options)

    for (i in 1:base::length(newNames)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 3:(base::ncol(WS) - base::length(opts))) {
      WS[, (base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 3):(base::length(opts) + base::length(opts) + 2)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 3):(base::length(opts) + base::length(opts) + 2)])) * 100
    }


    HOT <- WS[, c("id", "choice", Perc)]

    HOT$pred <- 0

    for (i in 1:base::nrow(HOT)) {
      for (k in 3:(base::ncol(HOT) - 1)) {
        if (HOT[i, k] == base::max(HOT[i, 3:(base::ncol(HOT) - 1)])) {
          HOT$pred[i] <- k - 2
        }
      }
    }

    HOT$MHP <- 0
    for (i in 1:base::nrow(HOT)) {
      HOT$MHP[i] <- HOT[i, (HOT$choice[i] + 2)]
    }

    MeanHIT <- base::as.data.frame(base::mean(HOT$MHP))

    colnames(MeanHIT) <- "MeanHitProb"
    return(MeanHIT)

  }

  if (!(base::is.null(Group))) {
    Options <- c()

    for (k in 1:base::length(opts)) {
      name <- base::paste0("Option_", k)
      Options <- c(Options, name)
    }

    newNames <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Opt_", k)
      newNames <- c(newNames, name)
    }

    Perc <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Perc_", k)
      Perc <- c(Perc, name)
    }

    base::colnames(WS) <- c("id", "Group", "choice", Options)

    for (i in 1:base::length(newNames)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 4:(base::ncol(WS) - base::length(opts))) {
      WS[, (base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 3)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 3)])) * 100
    }


    HOT <- WS[, c("id", "choice", "Group", Perc)]

    HOT$pred <- 0

    for (i in 1:base::nrow(HOT)) {
      for (k in 4:(base::ncol(HOT) - 1)) {
        if (HOT[i, k] == base::max(HOT[i, 4:(base::ncol(HOT) - 1)])) {
          HOT$pred[i] <- k - 3
        }
      }
    }

    HOT$MHP <- 0
    for (i in 1:base::nrow(HOT)) {
      HOT$MHP[i] <- HOT[i, (HOT$choice[i] + 3)]
    }

    MHP <- base::rbind(HOT %>%
                        dplyr::summarise(Group = "All",
                                         MeanHitProb = base::mean(MHP)) %>%
                        base::as.data.frame(),
                      HOT %>%
                        dplyr::group_by(Group) %>%
                        dplyr::summarise(MeanHitProb = base::mean(MHP)) %>%
                        base::as.data.frame())

    # fixing grouping variable

    lab <- c()

    if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_num <- base::sort(base::unique(WS$Group))

        lab <- c(lab, lab_num[i])

      }
    }

    if (base::is.character(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_char <- base::sort(base::unique(WS$Group))

        lab <- c(lab, lab_char[i])

      }
    }


    if (base::is.factor(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_fac <- base::sort(base::unique(WS$Group))

        lab <- c(lab, base::levels(lab_fac)[i])

      }
    }

    if (labelled::is.labelled(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_lab <- base::sort(base::unique(WS$Group))

        lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])

      }
    }




    MHP$Group <- lab

    return(MHP)


  }
}
