#' Calculate grassland biomass split between rangelands and pasture demand.
#'
#' Calculates pasture biomass demand for the historical period split between rangelands and managed pastures.
#' @return Regional biomass demand
#' @author Marcos Alves
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}},
#' \code{\link{readSource}}
#' @examples
#' \dontrun{
#' calcOutput("GrasslandBiomass")
#' }
#'
calcGrasslandBiomass <- function() {
  magYearsPast <- findset("past")
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
  biomassSplit <- toolCountryFill(biomassSplit, fill = 0)

  return(list(
    x = biomassSplit,
    weight = NULL,
    isocountries = FALSE,
    unit = "ton DM per ha",
    description = "Pasture biomass demand"
  ))
}
