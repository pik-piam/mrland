#' @title readProtectedAreaBaseline
#' @description Reads spatial land cover information within protected areas. Land cover information for protected areas has been extracted from ESA CCI land use/land cover data (https://www.esa-landcover-cci.org/) and data from the WDPA data base (https://www.protectedplanet.net).
#' @return Returns magpie object with the protected area separated for each land type (cropland, pasture, forest, other land) per grid cell from 1995 to 2020.
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' readSource("ProtectedAreaBaseline", convert = "onlycorrect")
#' }
#' @importFrom terra aggregate cellSize classify extract rast segregate terraOptions tmpFiles
#' @importFrom mrcommons toolGetMappingCoord2Country
#' @importFrom withr local_tempdir defer
#'

readProtectedAreaBaseline <- function() {

  # Set up 'terra' options
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terraOptions(tempdir = tempdir()))

  # Historic reference years
  refyears <- c(1995, 2000, 2005, 2010, 2015, 2020)

  # create output object
  out <- NULL

  for (yr in refyears) {
    message(paste("Beginning year", yr))

    # Read raster file: The file contains land cover information for all legally
    # designated protected areas in the reference year, estimated based on
    # ESA CCI land use/land cover (LULC)
    luWDPA_area_0.5 <- rast(paste0("./wdpa_esacci_land_", yr, "_0.5deg.tif"))

    # get spatial mapping
    map <- toolGetMappingCoord2Country(pretty = TRUE)
    # transform raster to magpie object
    luWDPA_lpj0.5 <- mbind(
      as.magpie(extract(luWDPA_area_0.5[["crop"]], map[c("lon", "lat")])[, "crop"], spatial = 1),
      as.magpie(extract(luWDPA_area_0.5[["past"]], map[c("lon", "lat")])[, "past"], spatial = 1),
      as.magpie(extract(luWDPA_area_0.5[["forest"]], map[c("lon", "lat")])[, "forest"], spatial = 1),
      as.magpie(extract(luWDPA_area_0.5[["other"]], map[c("lon", "lat")])[, "other"], spatial = 1)
    )
    # set dimension names
    dimnames(luWDPA_lpj0.5) <- list(
      "x.y.iso" = paste(map$coords, map$iso, sep = "."),
      "t" = paste0("y", yr),
      "data" = c("crop", "past", "forest", "other")
    )

    # bind to output
    out <- mbind(out, luWDPA_lpj0.5)

    message(paste("Finished year", yr))

  }

  return(out)
}
