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
    # ESA CCI land use/land cover (LULC) information with a spatial resolution of
    # 10 arc seconds
    luWDPA <- rast(paste0("./ESACCI-LC_WDPA_", yr, "_10s.tif"))

    # The LULC information was simplified to the following classes:
    # urban = "1", crop = "2", past = "3", forest = "4", other = "5", water = "6", barren = "7"

    # set "urban" (1) and "water" (6) to NA, add "barren" to "other" (5)
    classMatrx_lc <- rbind(c(1, NA), c(6, NA), c(7, 5))
    luWDPA <- classify(luWDPA, classMatrx_lc)
    gc()

    # divide into separate layers
    luWDPA <- segregate(luWDPA, other = NA)
    gc()

    # compute cell size for each LULC layer
    luWDPA_area <- cellSize(luWDPA[[c("2", "3", "4", "5")]], mask = TRUE, unit = "ha")
    gc()
    # set names
    names(luWDPA_area) <- c("crop", "past", "forest", "other")

    # sum cell area to 0.5 degree
    # aggregation factor from 10 arc sec to 0.5 degree: 180
    luWDPA_area_0.5 <- aggregate(luWDPA_area, fact = 180, fun = sum, na.rm = TRUE)
    gc()
    # convert to Mha
    luWDPA_area_0.5 <- luWDPA_area_0.5 / 1e6

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

    rm(luWDPA, luWDPA_area, luWDPA_area_0.5)
    tmpFiles(current = TRUE, orphan = TRUE, remove = FALSE)
    gc()
  }

  return(out)
}
