#' @title calcForestLossByDriver
#'
#' @description Forest area lost per year by driver, in Mha, from the Global Forest Watch driver
#' product of Sims et al. (2025) (\code{src = "GFW"}, default) or from Table 1 of Curtis et al.
#' (2018) (\code{src = "Curtis"}). Only the driver classes passed on to MAgPIE are returned.
#'
#' @details Curtis et al. (2018) report loss for seven world regions. It is distributed to
#' countries by FRA 2020 naturally regenerating forest area in 2010.
#'
#' @param src \code{"GFW"} (default) or \code{"Curtis"}
#' @param period Years averaged into the annual rate, GFW only; Curtis carries a single 2001-2015
#' mean
#' @return MAgPIE object with forest area lost per year by driver, Mha
#' @author Abhijeet Mishra, Michael Crawford
#' @seealso \code{\link{readGFWLossByDriver}}, \code{\link{readForestLossDrivers}}
#' @examples
#' \dontrun{
#' calcOutput("ForestLossByDriver", aggregate = FALSE)
#' calcOutput("ForestLossByDriver", src = "Curtis", aggregate = FALSE)
#' }

calcForestLossByDriver <- function(src = "GFW", period = 2015:2024) {

  # Driver classes passed on to MAgPIE, each needing an element in MAgPIE's driver_source set.
  # GFW options: permanent_agriculture, hard_commodities, shifting_cultivation, logging, wildfire,
  # settlements_infrastructure, other_natural_disturbances, unknown. Curtis shares only
  # shifting_cultivation and wildfire with these names.
  # Only shifting cultivation is passed on: it leaves the land as forest and MAgPIE does not model
  # it itself. Clearing for agriculture, commodities and settlements removes the forest, logging
  # is MAgPIE's own harvest, wildfire and other natural disturbances are natural processes.
  modelDrivers <- c("shifting_cultivation")

  out <- switch(
    src,
    "Curtis" = {
      x <- readSource("ForestLossDrivers", convert = FALSE)
      getItems(x, dim = 3)[getItems(x, dim = 3) == "shifting_agriculture"] <- "shifting_cultivation"
      mapping <- toolGetMapping("regionmappingCurtis2018.csv", type = "regional", where = "mrland")
      # the area and year calcForestLossShare divides by, so every country in a region carries the
      # region's rate
      weight <- readSource("FRA2020", "forest_area", convert = TRUE)[, "y2010", "naturallyRegeneratingForest"]
      toolAggregate(x[, , modelDrivers], rel = mapping, weight = setYears(collapseNames(weight), NULL),
                    from = "RegionCode", to = "CountryCode")
    },
    "GFW" = {
      x <- readSource("GFWLossByDriver", convert = TRUE)
      wanted <- paste0("y", period)
      absent <- setdiff(wanted, getYears(x))
      if (length(absent) > 0) {
        stop("calcForestLossByDriver: the GFW record does not cover ", toString(absent),
             ". It runs ", min(getYears(x, TRUE)), "-", max(getYears(x, TRUE)), ".")
      }
      dimSums(x[, wanted, modelDrivers], dim = 2) / length(wanted)
    },
    stop("calcForestLossByDriver: unknown src '", src, "'. Use \"GFW\" or \"Curtis\".")
  )

  return(list(x = out,
              weight = NULL,
              unit = "Mha",
              description = paste0("Forest area lost per year by driver (", src, ")"),
              isocountries = FALSE))
}
