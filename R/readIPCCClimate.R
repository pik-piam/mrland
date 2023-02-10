#' @title readIPCCClimate
#' @description Read IPCC climate classification
#'
#' @return Magpie object with results on cellular level for 12 IPCC climate zone types
#' @author  Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("IPCCClimate", convert = "onlycorrect")
#' }
#'
#' @importFrom raster raster aggregate extract levels values values<-

readIPCCClimate <-  function() {

  raster1d12   <- raster("CLIMATE_ZONE.rst")
  raster::values(raster1d12)[raster::values(raster1d12) == 0] <- NA
  zoneNames    <- as.character(levels(raster1d12)[[1]]$Class_name)
  raster1d2    <- raster::aggregate(raster1d12, fact = 6, fun = "modal", na.rm = TRUE)

  map           <- as.data.frame(magpie_coord)
  mag           <- as.magpie(raster::extract(raster1d2, map), spatial = 1)
  cellNames     <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")$celliso
  getNames(mag) <- "NA"
  getYears(mag) <- NULL
  getCells(mag) <- cellNames
  getSets(mag)  <- c("country.cell", "t", "climatezone")
  mag[is.na(mag)] <- 0

  out   <- add_columns(mag, dim = 3.1, addnm = zoneNames)
  out[] <- 0
  for (zone in c(zoneNames)) {
    out[, , zone][which(mag == which(zoneNames == zone))] <- 1
  }
  out[, , "NA"][which(dimSums(out[, , "NA", invert = TRUE], dim = 3) == 0)] <- 1
  out <- mbind(out[, , zoneNames], out[, , "NA"])

  return(out)
}
