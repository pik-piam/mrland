#' @title readNoon2022
#' @description Reads irrecoverble carbon data set published by Noon, M. L., Goldstein, A.,
#' Ledezma, J. C., Roehrdanz, P. R., Cook-Patton, S. C., Spawn-Lee, S. A., Wright, T. M.,
#' Gonzalez-Roglich, M., Hole, D. G., Rockström, J., & Turner, W. R. (2022). Mapping the
#' irrecoverable carbon in Earth’s ecosystems. Nature Sustainability, 5(1),
#' Article 1. https://doi.org/10.1038/s41893-021-00803-6
#' @param subtype Defines whether carbon data or land area and related subtypes should be returned (see options below).
#' Carbon or land subtypes need to be specified via ":"
#' The different subtypes for land are:
#' \itemize{
#' \item \code{"IrrecovCarbon50"}: Unprotected land (specified for different land types) that covers the
#' highest 50 % of all irrecoverable carbon stocks, as defined in Noon et al. (2022).
#' \item \code{"IrrecovCarbon75"}: Unprotected land (specified for different land types) that covers the
#' highest 75 % of all irrecoverable carbon stocks.
#' \item \code{"IrrecovCarbonAll"}: Unprotected land (specified for different land types) that covers
#' all irrecoverable carbon stocks.
#' }

#' @return Returns magpie objects with the area of unprotected irrecoverable carbon land per grid cell
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

readNoon2022 <- function(subtype = "land:IrrecovCarbon50") {

  # extract subtype
  datatype <- unlist(strsplit(subtype, split = ":"))[1]
  subtype <- unlist(strsplit(subtype, split = ":"))[2]

  # Set up 'terra' options
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terraOptions(tempdir = tempdir()))

  if (datatype == "land") {

    # read data
    if (subtype == "IrrecovCarbon50") {
      unprotectedICLand <- rast(paste0(
        "./unprotected_irrecoverable_C_land",
        "/Irrecoverable_C_50perc_unprotected_land_area_0.5deg.tif"
      ))
    } else if (subtype == "IrrecovCarbon75") {
      unprotectedICLand <- rast(paste0(
        "./unprotected_irrecoverable_C_land",
        "/Irrecoverable_C_75perc_unprotected_land_area_0.5deg.tif"
      ))
    } else if (subtype == "IrrecovCarbonAll") {
      unprotectedICLand <- rast(paste0(
        "./unprotected_irrecoverable_C_land",
        "/Irrecoverable_C_all_unprotected_land_area_0.5deg.tif"
      ))
    } else {
      stop("Please select an existing subtype")
    }

    # get spatial mapping
    map <- toolGetMappingCoord2Country(pretty = TRUE)
    # transform raster to magpie object
    out <- mbind(
      as.magpie(extract(unprotectedICLand[["crop"]], map[c("lon", "lat")])[, "crop"], spatial = 1),
      as.magpie(extract(unprotectedICLand[["past"]], map[c("lon", "lat")])[, "past"], spatial = 1),
      as.magpie(extract(unprotectedICLand[["forest"]], map[c("lon", "lat")])[, "forest"], spatial = 1),
      as.magpie(extract(unprotectedICLand[["other"]], map[c("lon", "lat")])[, "other"], spatial = 1)
    )

    # set dimension names
    dimnames(out) <- list(
      "x.y.iso" = paste(map$coords, map$iso, sep = "."),
      "t" = NULL,
      "data" = paste(subtype, c("crop", "past", "forest", "other"), sep = ".")
    )

    return(out)
  }
}
