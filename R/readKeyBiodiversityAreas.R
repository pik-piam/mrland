#' @title readKeyBiodiversityAreas
#' @description Reads land area covered by for Key Biodiversity Areas
#' (https://www.keybiodiversityareas.org/) that was unprotected in 2020.
#' Protected areas were masked at a spatial resolution of 10 arc seconds
#' before aggregating the data to 0.5°.
#'
#' @return Returns magpie objects with the area covered by unprotected Key Biodiversity Areas per grid cell
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' readSource("KeyBiodiversityAreas", convert = "onlycorrect")
#' }
#'
#' @importFrom terra rast classify cellSize aggregate terraOptions
#' @importFrom withr local_tempdir defer
#' @importFrom mrcommons toolGetMappingCoord2Country
#'

readKeyBiodiversityAreas <- function() {

  # Set up 'terra' options
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terraOptions(tempdir = tempdir()))

  unprotectedKBALand <- rast("./kba_land_unprotected_0.5.tif")

  # get spatial mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  out <- mbind(
    as.magpie(extract(unprotectedKBALand[["crop"]], map[c("lon", "lat")])[, "crop"], spatial = 1),
    as.magpie(extract(unprotectedKBALand[["past"]], map[c("lon", "lat")])[, "past"], spatial = 1),
    as.magpie(extract(unprotectedKBALand[["forest"]], map[c("lon", "lat")])[, "forest"], spatial = 1),
    as.magpie(extract(unprotectedKBALand[["other"]], map[c("lon", "lat")])[, "other"], spatial = 1)
  )

  # set dimension names
  dimnames(out) <- list(
    "x.y.iso" = paste(map$coords, map$iso, sep = "."),
    "t" = NULL,
    "data" = c("crop", "past", "forest", "other")
  )

  return(out)
}
