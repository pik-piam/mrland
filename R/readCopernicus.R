#' @title readCopernicus
#' @description Reads either information on the area on cropland covered by trees or
#' information the cropland area that requires relocation in response of increasing
#' semi-natural vegetation in farmed landscapes. The data was derived from high resolution
#' land cover information (LC100) from the Copernicus Global Land Service.
#' (https://zenodo.org/records/3939050)
#'
#' @param subtype For cropland area covered by trees choose \code{"CroplandTreecover"}.
#' For cropland area requiring relocation in response to increasing SNV choose \code{"SNVTargetCropland"}.
#'
#' @return Returns magpie objects with cropland area covered by trees or cropland area
#' requiring relocation in order to increase SNV in farmed landscapes.
#'
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' readSource("Copernicus", subtype = "CroplandTreecover", convert = "onlycorrect")
#' }
#'
#' @importFrom terra rast terraOptions
#' @importFrom withr local_tempdir defer
#' @importFrom mrcommons toolGetMappingCoord2Country
#'

readCopernicus <- function(subtype = "CroplandTreecover") {
  # Set up 'terra' options
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terraOptions(tempdir = tempdir()))

  # read data
  if (subtype == "CroplandTreecover") {
    r <- rast(paste0(
      "./land_cover/copernicus_lc100_2019_crop_treecover_area_0.5.tif"
    ))
    name <- "CropTreecoverArea"
  } else if (subtype == "SNVTargetCropland") {
    r <- rast(paste0(
      "./land_cover/copernicus_lc100_2019_SNV50_target_crop_area_0.5.tif"
    ))
    name <- "SNVTargetCropland"
  } else {
    stop("Please select an existing subtype")
  }

  # get spatial mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  out <- mbind(
    as.magpie(extract(r, map[c("lon", "lat")], ID = FALSE), spatial = 1)
  )

  # set dimension names
  dimnames(out) <- list(
    "x.y.iso" = paste(map$coords, map$iso, sep = "."),
    "t" = NULL,
    "data" = name
  )

  return(out)
}
