#' Root Mean Squared Error
#'
#' @description Function to measure the mean absolute error of a holdout task
#' @param data a data frame
#' @param id column index of the \code{id} variable
#' @param Group optional grouping variable to get hit rate by group
#' @param opts column indexes of the options included in the holdout task
#' @param choice column index of the actual choice
#'
#' @return xyz
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#'
#' @export

RMSE <- function(data, id, Group = NULL, opts, choice){

  WS <- data[, c(id, Group, choice, opts)]

  Count <- NULL

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

    Helper <- base::as.data.frame(base::matrix(nrow = base::length(3:(base::ncol(HOT) - 1)), ncol = 1))

    base::colnames(Helper) <- "Options"

    Helper$Options <- c(1:base::length(3:(base::ncol(HOT) - 1)))

    Actual <- HOT %>%
      dplyr::group_by(choice) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Share = Count / base::sum(Count) * 100) %>%
      base::as.data.frame()

    Predicted <- base::data.frame(base::cbind(c(1:base::length(3:(base::ncol(HOT) - 1))),
                                              c(base::unname(base::colMeans(HOT[, c(3:(base::ncol(HOT) - 1))])))))

    base::colnames(Predicted) <- c("Options", "Pred")

    RMSE <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x=T)

    RMSE <- base::merge(x = RMSE, y = Predicted, by = "Options", all.x=T)

    base::print(base::sqrt((base::sum((base::abs(RMSE$Share - RMSE$Pred))^2)/base::length(opts))))


  }

  if (!(base::is.null(Group))){

    Options <- c()

    for (k in 1:base::length(opts)){
      name <- base::paste0("Option_", k)
      Options <- c(Options, name)
    }

    newNames <- c()
    for (k in 1:base::length(opts)){
      name <- base::paste0("Opt_", k)
      newNames <- c(newNames, name)
    }

    Perc <- c()
    for (k in 1:base::length(opts)){
      name <- base::paste0("Perc_", k)
      Perc <- c(Perc, name)
    }

    base::colnames(WS) <- c("id", "Group", "choice", Options)

    for (i in 1:base::length(newNames)){
      WS[ , base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 4:(base::ncol(WS) - base::length(opts))){
      WS[,(base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)){
      WS[ , base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 3)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 4)])) * 100
    }


    HOT <- WS[, c("id", "choice", "Group", Perc)]

    HOT$pred <- 0

    for (i in 1:base::nrow(HOT)){
      for (k in 4:(base::ncol(HOT) - 1)) {
        if (HOT[i, k] == base::max(HOT[i, 4:(base::ncol(HOT) - 1)])) {
          HOT$pred[i] <- k - 3
        }
      }
    }

    RMSE <- base::data.frame(Group=base::character(base::length(base::unique(HOT$Group)) + 1),RMSE=base::numeric(base::length(base::unique(HOT$Group)) + 1))

    for (p in 1:base::length(base::unique(HOT$Group))){
      if (p == 1) {
        Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(HOT) - 1)), ncol = 1))

        base::colnames(Helper) <- "Options"

        Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))

        Actual <- HOT %>%
          dplyr::group_by(choice) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Share = Count / sum(Count) * 100) %>%
          base::as.data.frame()


        Predicted <- base::data.frame(base::cbind(c(1:base::length(4:(base::ncol(HOT) - 1))),
                                                  c(base::unname(base::colMeans(HOT[, c(4:(base::ncol(HOT) - 1))])))))

        base::colnames(Predicted) <- c("Options", "Pred")

        DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x=T)

        DataFrame <- base::merge(x = DataFrame, y = Predicted, by = "Options", all.x=T)

        RMSE[p, p] <- "All"

        RMSE[p, (p + 1)] <- base::sqrt((base::sum((base::abs(DataFrame$Share - DataFrame$Pred))^2)/base::length(opts)))

        base::rm(Helper, Actual, Predicted, DataFrame)

      }

      Group <- base::subset(HOT, base::as.character(Group) == base::unique(base::as.character(HOT$Group))[p])

      Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(Group) - 1)), ncol = 1))

      base::colnames(Helper) <- "Options"

      Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))


      Actual <- Group %>%
        dplyr::group_by(choice) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Share = Count / base::sum(Count) * 100) %>%
        base::as.data.frame()

      Predicted <- base::data.frame(base::cbind(c(1:base::length(4:(base::ncol(Group) - 1))),
                                                c(base::unname(base::colMeans(Group[, c(4:(base::ncol(Group) - 1))])))))

      base::colnames(Predicted) <- c("Options", "Pred")

      DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x=T)

      DataFrame <- base::merge(x = DataFrame, y = Predicted, by = "Options", all.x=T)

      RMSE[(p + 1), 1] <- base::unique(base::as.character(HOT$Group))[p]

      RMSE[(p + 1), 2] <- base::sqrt((base::sum((base::abs(DataFrame$Share - DataFrame$Pred))^2)/base::length(opts)))

      base::rm(Helper, Actual, Predicted, DataFrame)

      if (p == base::max(base::length(base::unique(HOT$Group)))){
        return(RMSE)
      }
    }
  }
}
