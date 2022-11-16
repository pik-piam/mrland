#' @title readNoon2022
#' @description Reads irrecoverble carbon data set published by Noon, M. L., Goldstein, A.,
#' Ledezma, J. C., Roehrdanz, P. R., Cook-Patton, S. C., Spawn-Lee, S. A., Wright, T. M.,
#' Gonzalez-Roglich, M., Hole, D. G., Rockström, J., & Turner, W. R. (2022). Mapping the
#' irrecoverable carbon in Earth’s ecosystems. Nature Sustainability, 5(1),
#' Article 1. https://doi.org/10.1038/s41893-021-00803-6

#' @return Returns magpie objects with the area of irrecoverable carbon land per grid cell
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' readSource("Noon2022", convert = "onlycorrect")
#' }
#'
#' @importFrom terra rast classify cellSize aggregate terraOptions
#' @importFrom withr local_tempdir defer
#' @importFrom mrcommons toolGetMappingCoord2Country
#'

readNoon2022 <- function() {

  # Set up 'terra' options
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terraOptions(tempdir = tempdir()))

  # read data
  icStocks <- rast("./Irrecoverable_C_Stocks_Total_2018_10s.tif")

  # Reclassify pixels that make up the top 50 % of global irrecoverable carbon
  # The threshold is 642 MgC
  icStocksHalf <- classify(icStocks, cbind(0, 642, NA), include.lowest = TRUE)

  # Reclassify pixels that make up the top 75 % of global irrecoverable carbon
  # The threshold is 362 MgC
  icStocks3Q <- classify(icStocks, cbind(0, 362, NA), include.lowest = TRUE)

  # Reclassify pixels that make up the top 100 % of global irrecoverable carbon
  # Therefore exclude all pixels that are zero
  icStocksAll <- classify(icStocks, cbind(0, NA), include.lowest = TRUE)

  gc()

  # divide into separate layers
  icLayers <- c(icStocksHalf, icStocks3Q, icStocksAll)

  # compute cell size for each LULC layer
  icLayers <- cellSize(icLayers, mask = TRUE, unit = "ha")

  # sum cell area to 0.5 degree
  # aggregation factor from 10 arc sec to 0.5 degree: 180
  icLayers05 <- aggregate(icLayers, fact = 180, fun = sum, na.rm = TRUE)
  gc()
  # convert to Mha
  icLayers05 <- icLayers05 / 1e6

  names(icLayers05) <- c("IC_50perc", "IC_75perc", "IC_all")

  # get spatial mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  out <- mbind(
    as.magpie(extract(icLayers05[["IC_50perc"]], map[c("lon", "lat")])[, "IC_50perc"], spatial = 1),
    as.magpie(extract(icLayers05[["IC_75perc"]], map[c("lon", "lat")])[, "IC_75perc"], spatial = 1),
    as.magpie(extract(icLayers05[["IC_all"]], map[c("lon", "lat")])[, "IC_all"], spatial = 1)
  )
  # set dimension names
  dimnames(out) <- list(
    "x.y.iso" = paste(map$coords, map$iso, sep = "."),
    "t" = NULL,
    "data" = c("IC_50perc", "IC_75perc", "IC_all")
  )

  return(out)
}
