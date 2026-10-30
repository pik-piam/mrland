#' @title readForestGrassi2023
#' @description Reads the 0.5 degree map of intact and non-intact forest area of Grassi et al. (2023),
#' Harmonising the land-use flux estimates of global models and national inventories for 2000-2020,
#' Earth Syst. Sci. Data 15, 1093-1114, https://doi.org/10.5194/essd-15-1093-2023.
#' Intact forest follows Potapov et al. (2017) for 2013, with the masks for Canada, Brazil and Russia
#' updated from national information; non-intact forest is forest after Hansen et al. (2013) minus intact forest.
#'
#' @return magpie object with intact and non-intact forest area (Mha) on the 67420 cells
#' @author Florian Humpenöder
#' @seealso \code{\link{downloadForestGrassi2023}}, \code{\link{correctForestGrassi2023}}
#' @examples
#' \dontrun{
#' readSource("ForestGrassi2023", convert = "onlycorrect")
#' }
#' @importFrom terra rast extract
#' @importFrom mstools toolGetMappingCoord2Country

readForestGrassi2023 <- function() {

  map <- toolGetMappingCoord2Country(pretty = TRUE)
  layers <- c(intact = "IntactForestArea", nonintact = "NonIntactForestArea")

  out <- NULL
  for (l in names(layers)) {
    r <- rast("IntactAndNonIntactForest_0.5deg.nc", subds = layers[[l]])
    # m2 to Mha
    x <- as.magpie(extract(r, map[c("lon", "lat")])[, 2] / 1e10, spatial = 1)
    out <- mbind(out, setNames(x, l))
  }
  dimnames(out) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."),
                        "t" = NULL,
                        "data" = names(layers))
  return(out)
}
