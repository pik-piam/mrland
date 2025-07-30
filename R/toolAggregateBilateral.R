#' @title toolAggregateBilateral
#' @description This tool aggregates a spatially bilateral magpie object (trade) 
#' using optionally one weight for both dimensions, basically runs toolAggregate twice
#' @param data      time series of the scenario
#' @param weight mean of scenario time series

#' @return aggregated bilateral object
#' @author david chen
#'
#' @importFrom magclass is.magpie new.magpie getCells getYears getSets
#' @importFrom madrat toolConditionalReplace
#'
#' @export

toolAggregateBilateral <- function(data, weight = NULL) {

  if (!is.magpie(scen) || !is.magpie(scenMean) || !is.magpie(refMean)) {
    stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  }

  # check for negative range of values
  negative <- (any(scen < 0) | any(scenMean < 0) | any(refMean < 0))

  # set years
  years    <- getYears(scen, as.integer = TRUE)
  afterRef <- paste0("y", years[years >= as.numeric(substring(refYear, 2))])

  # check if all objects contain ref year
  if (!(refYear %in% Reduce(intersect, list(getYears(scen), getYears(scenMean), getYears(refMean))))) {
    stop("Reference year is not included in all time series provided.")
  }

  # check if x and base are identical in dimension except time
  # TO-DO find a way of muliple checking

  # create new magpie object with full time horizon
  out      <- new.magpie(getCells(scen), afterRef, getNames(scen), sets = getSets(scen))

  scen     <- scen[, afterRef, ]
  scenMean <- setYears(scenMean[, refYear, ], NULL)
  refMean  <- setYears(refMean[,  refYear, ], NULL)

  ###########################################
  ### Use DELTA-approach to put signal of ###
  ### GCM data on historical observation  ###
  ### data from reference year +1 on      ###
  ###########################################

  lambda <- sqrt(scenMean / refMean)
  lambda[scenMean >= refMean] <- 1
  lambda[is.nan(lambda)]      <- 1

  out <- (1 + (refMean - scenMean) / scen * toolConditionalReplace(scen / scenMean,
                                                                     c("is.na()", "is.infinite()"), 1)**lambda)

  if (any((is.infinite(out) | is.na(out)) & scen != 0)) stop("Data containing inconsistencies.")
  out[is.na(out)]        <- 0
  out[is.infinite(out)]  <- 0
  out <- scen * out

  # check for nans and more
  if (any(is.infinite(out) | is.nan(out) | is.na(out))) warning("Data containing inconsistencies.")
  if (!negative && any(out < 0)) {
    message(paste0("toolPatternScaling created unwanted negativities in the range of ",
                   range(out[which(out < 0)]), ". They will be set to zero."))
    out[out < 0] <- 0
  }

  return(out)
}
