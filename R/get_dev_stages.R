#' @title Get developmental stages from simulations
#'
#' @description Extracts the values of developmental stages from simulations
#'
#' @param sim A list, where each element is a `data.frame` of simulation results
#' for the given usm. The list is named after the USM name.
#'
#' @param dev_vars Vector of output variables corresponding to developmental stages.
#'
#' @param usm Vector of USM names. Optional, if not provided, the function
#' returns the results for all USMs.
#'
#' @return List of data.frame, one per USM, with one row per developmental stage.
#'
#' @seealso \code{\link{get_sim}}
#'
#' @author Timothee Flutre
#'
#' @export
#'
get_dev_stages <- function(sim,
                           dev_vars  =
                             c("iplts", "imbs", "ilets", "igers",
                               "idebdorms", "ifindorms",
                               "ilevs", "iamfs", "ilaxs", #"imontaisons",
                               "ilats", "iflos", "idrps", "inous", "idebdess", #"ilans",
                               "imats", "irecs"),
                           usm = NULL){
  stopifnot(is.list(sim),
            ! is.null(names(sim)))

  out <- list()

  if(is.null(usm))
    usm <- names(sim)
  stopifnot(all(usm %in% names(sim)))

  for(usm_name in usm){
    dat <- sim[[usm_name]]
    firstYear <- as.integer(sort(unique(format(dat$Date, format="%Y")))[1])
    out_usm <- data.frame(var = dev_vars,
                          day = NA,
                          date = "")
    for(i in 1:nrow(out_usm)){
      dev_var <- dev_vars[i]
      if(! dev_var %in% colnames(dat)){
        msg <- paste0("'", dev_var, "' not found")
        warning(msg, immediate. = TRUE)
        next
      }
      tmp <- table(dat[[dev_var]])
      if(length(tmp) == 1){ # ex. iplts
        out_usm$day[i] <- as.integer(names(tmp)[1])
      } else if(length(tmp) == 2){
        stopifnot(names(tmp)[1] == "0")
        out_usm$day[i] <- as.integer(names(tmp)[2])
      } else
        stop(paste0(dev_var, " has more than three values"))
      out_usm$date[i] <- as.character(compute_date_from_day(out_usm$day[i], firstYear))
    }
    out_usm$date <- as.Date(out_usm$date)
    out_usm$Date <- as.POSIXct(out_usm$date) # required to plot with CroPlotR
    out_usm$var <- factor(out_usm$var,
                          levels = out_usm$var[order(out_usm$day)],
                          ordered = TRUE)
    out[[usm_name]] <- out_usm
  }

  return(out)
}
