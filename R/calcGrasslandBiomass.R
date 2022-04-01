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

  # India, Bangladesh, and Pakistan have livestock numbers unusually high, in comparison to their
  # estimated grazing lands leading to very high grassland biomass demand. A share of this increased
  # demand can be fulfilled by alternative feed sources that are not accounted for in MAgPIE, such as
  # roadside grazing. To reduce demand/area ratio, I'm adding to the areas of managed pastures the category
  # "other land" that is assumed to be used for grazing in those three countries.

  grasslLand["IND", , "pastr"] <- grasslLand["IND", , "pastr"] + setNames(dimSums(land["IND",,c("primother","secdother")], dim = 3), "pastr")
  grasslLand["BGD", , "pastr"] <- grasslLand["BGD", , "pastr"] + setNames(dimSums(land["BGD",,c("primother","secdother")], dim = 3), "pastr")
  grasslLand["PAK", , "pastr"] <- grasslLand["PAK", , "pastr"] + setNames(dimSums(land["PAK",,c("primother","secdother")], dim = 3), "pastr")

  grasslShares <- setNames(grasslLand[, , "pastr"] / dimSums(grasslLand, dim = 3), "pastr")
  grasslShares <- add_columns(grasslShares, addnm = "range", dim = 3.1)
  grasslShares[is.nan(grasslShares) | is.infinite(grasslShares)] <- 0
  grasslShares[, , "range"] <- 1 - grasslShares[, , "pastr"]

  mapping <- toolGetMapping("CountryToCellMapping.rds", where = "mrcommons")

  livestock <- setNames(toolCell2isoCell(readSource("GLW3")), "liv_numb")

  # I am working with the assumption that in most places, the proportional distribution of
  # livestock heads was kept the same between 1965 and 2010. One notable exception is Brasil,
  # which experienced a shift in its agricultural frontier towards the west with increases in
  # rangelands used for cattle production. A "fader" is used to correct the proportional share
  # of livestock being in rangelands and managed pastures, and avoid distortions on the amount
  # of grass biomass production assigned to each system.


  livstSplit <- livestock * grasslShares

  # Fader calculation
  fader <- livstSplit
  fader[,,] <- 1
  start_pastr <- 1.8
  end_pastr <- 1
  start_range <- 0.2
  end_range <- 1
  range_values <- seq(start_range,end_range,(end_range-start_range)/(length(getItems(fader, dim = 2))-1))
  pastr_values <- seq(start_pastr,end_pastr,(end_pastr-start_pastr)/(length(getItems(fader, dim = 2))-1))
  for (i in 1:length(range_values)) {
    fader["BRA",i,"range"] <-  range_values[i]
  }
  for (i in 1:length(pastr_values)) {
    fader["BRA",i,"pastr"] <-  pastr_values[i]
  }
  livstSplit <-  livstSplit * fader

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
