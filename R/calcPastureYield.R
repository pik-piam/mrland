#' Calculate Pasture Yields
#'
#' Provides Pasture yields defined as ratio of grazed biomass to grazed area
#' @param cellular Boolean value indicating if the grass yields should be disaggregated on a 0.5 degree level.
#' @param max_yields Maximum yields in tDM/ha allowed in a cell.
#' @param max_iter Maximum number of iterations of the disaggregation algorithm
#' @description `max_yields` and `max_iter` are only affecting the calculations if `cellular` is TRUE.
#' @return Pasture yields and corresponding weights as a list of
#' two MAgPIE objects
#' @author Isabelle Weindl, Marcos Alves
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}},
#' \code{\link{readSource}}
#' @examples
#' \dontrun{
#' calcOutput("PastureYield")
#' }
#' @importFrom stats quantile

calcPastureYield <- function(cellular = FALSE, max_yields = 20, max_iter = 30) {

  mag_years_past <- findset("past")
  biomass <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production.dm"][, mag_years_past, "pasture"]
  biomass <- collapseNames(biomass)
  past.land <- calcOutput("LanduseInitialisation", aggregate = FALSE)[, mag_years_past, "past"]
  pstr_yield <- biomass / past.land
  pstr_yield[is.nan(pstr_yield)] <- 1
  pstr_yield[pstr_yield > 100] <- 100
  getNames(pstr_yield) <- NULL

  return(list(
    x = pstr_yield,
    weight = past.land,
    unit = "ton DM per ha",
    description = "Pasture yields"
  ))
}
