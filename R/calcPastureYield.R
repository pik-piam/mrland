#' Calculate Pasture Yields
#'
#' Provides Pasture yields defined as ratio of grazed biomass to grazed area
#' @param range_pastr Boolean value indicating if the grass yields should be split between rangelands and pastures.
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

calcPastureYield <- function(range_pastr = FALSE) { # nolint
  if (range_pastr) {
    magYearsPast <- findset("past")[c(7, 8, 9, 10)]
    biomass <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production.dm"][, magYearsPast, "pasture"]
    biomass <- collapseNames(biomass)

    biomass <- toolIso2CellCountries(biomass)
    land <- calcOutput("LanduseInitialisation", cellular = TRUE, nclasses = "nine", aggregate = FALSE)[, magYearsPast, ]
    grasslLand <- land[, , c("past", "range")]
    grasslLand <- setNames(grasslLand, c("pastr", "range"))
    grasslShares <- setNames(grasslLand[, , "pastr"] / dimSums(grasslLand, dim = 3), "pastr")
    grasslShares <- add_columns(grasslShares, addnm = "range", dim = 3.1)
    grasslShares[, , "range"] <- 1 - grasslShares[, , "pastr"]
    grasslShares[is.nan(grasslShares) | is.infinite(grasslShares)] <- 0

    mapping <- toolGetMapping("CountryToCellMapping.rds", where = "mrcommons")

    livestock <- setNames(toolCell2isoCell(readSource("GLW3")), "liv_numb")
    livstSplit <- livestock * grasslShares
    livstSplit <- collapseNames(livstSplit)
    livstSplitCtry <- toolAggregate(livstSplit, rel = mapping, to = "iso", from = "celliso")
    livstShareCtry <- livstSplitCtry[, , "pastr"] / dimSums(livstSplitCtry, dim = 3)
    livstShareCtry[is.nan(livstShareCtry) | is.infinite(livstShareCtry)] <- 0
    livstShareCtry <- add_columns(livstShareCtry, addnm = "range", dim = 3.1)
    livstShareCtry[, , "range"] <- 1 - livstShareCtry[, , "pastr"]

    # I am splitting biomass consumption assuming the share
    # between animals reared on rangelands and pastures correlates linearly
    # with the production of grass in pastures and rangelands in a country. That can be
    # derived by the fact that the feedbaskets assume the same feed ingredients shares
    # within a country.

    biomassSplit <- biomass * livstShareCtry
    grasslLandCtry <- toolAggregate(grasslLand, rel = mapping, to = "iso", from = "celliso")
    pstrYield <- biomassSplit / grasslLandCtry
    pstrYield[pstrYield > 100] <- 100
    pstrYield <- toolCountryFill(pstrYield)
    pstrYield[is.nan(pstrYield) | is.na(pstrYield)] <- 1
    grasslLandCtry <- toolCountryFill(grasslLandCtry)
    grasslLandCtry[is.na(grasslLandCtry)] <- 0
    return(list(
      x = pstrYield,
      weight = grasslLandCtry,
      isocountries = FALSE,
      unit = "ton DM per ha",
      description = "Pasture yields"
    ))
  }

  magYearsPast <- findset("past")
  biomass <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production.dm"][, magYearsPast, "pasture"]
  biomass <- collapseNames(biomass)
  pastLand <- calcOutput("LanduseInitialisation", aggregate = FALSE)[, magYearsPast, "past"]
  pstrYield <- biomass / pastLand
  pstrYield[is.nan(pstrYield)] <- 1
  pstrYield[pstrYield > 100] <- 100
  getNames(pstrYield) <- NULL

  return(list(
    x = pstrYield,
    weight = pastLand,
    unit = "ton DM per ha",
    description = "Pasture yields"
  ))
}
