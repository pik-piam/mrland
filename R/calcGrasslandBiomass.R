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
  mag_years_past <- findset("past")[c(7, 8, 9, 10)]
  biomass <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production.dm"][, mag_years_past, "pasture"]
  biomass <- collapseNames(biomass)
  biomass <- toolIso2CellCountries(biomass)

  land <- calcOutput("LanduseInitialisation", cellular = TRUE, nclasses = "nine", aggregate = FALSE)[, mag_years_past, ]
  land_total <- dimSums(land, dim = 3)
  grassl_land <- land[, , c("past", "range")]
  grassl_land <- setNames(grassl_land, c("pastr", "range"))
  grassl_shares <- setNames(grassl_land[, , "pastr"] / dimSums(grassl_land, dim = 3), "pastr")
  grassl_shares <- add_columns(grassl_shares, addnm = "range", dim = 3.1)
  grassl_shares[, , "range"] <- 1 - grassl_shares[, , "pastr"]
  grassl_shares[is.nan(grassl_shares) | is.infinite(grassl_shares)] <- 0

  mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")

  livestock <- setNames(toolCell2isoCell(readSource("GLW3")), "liv_numb")
  livst_split <- livestock * grassl_shares
  livst_split <- collapseNames(livst_split)
  livst_split_ctry <- toolAggregate(livst_split, rel = mapping, to = "iso", from = "celliso")
  livst_share_ctry <- livst_split_ctry[, , "pastr"] / dimSums(livst_split_ctry, dim = 3)
  livst_share_ctry[is.nan(livst_share_ctry) | is.infinite(livst_share_ctry)] <- 0
  livst_share_ctry <- add_columns(livst_share_ctry, addnm = "range", dim = 3.1)
  livst_share_ctry[, , "range"] <- 1 - livst_share_ctry[, , "pastr"]

  # I am splitting biomass consumption assuming the share
  # between animals reared on rangelands and pastures correlates linearly
  # with the production of grass in pastures and rangelands in a country. That can be
  # derived by the fact that the feedbaskets assume the same feed ingredients shares
  # within a country.

  biomass_split <- biomass * livst_share_ctry


  return(list(
    x = biomass_split,
    weight = NULL,
    isocountries = FALSE,
    unit = "ton DM per ha",
    description = "Pasture biomass demand"
  ))
}
