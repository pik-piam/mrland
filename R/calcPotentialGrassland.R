#' @title calcPotentialGrassland
#' @description Per-cell cover fraction of the RESOLVE Ecoregions 2017 grassland and savanna
#' biomes (Dinerstein et al. 2017): the share of each cell whose potential natural vegetation
#' is grassy (tropical and subtropical, temperate, flooded, and montane grasslands, savannas
#' and shrublands).
#'
#' @return magpie object in cellular resolution (67420 lpjcells)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("PotentialGrassland", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput

calcPotentialGrassland <- function() {

  grass <- readSource("Dinerstein2017", subtype = "grassland", convert = "onlycorrect")
  getNames(grass) <- NULL

  # land area weights the cluster/region aggregation of the cover fraction
  landArea <- calcOutput("LandArea", aggregate = FALSE)

  return(list(
    x           = grass,
    weight      = landArea,
    unit        = "fraction",
    description = "Potential natural grassland cover fraction (RESOLVE 2017 biomes 7-10)",
    isocountries = FALSE
  ))
}
