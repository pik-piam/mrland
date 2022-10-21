#' @title readGLW3
#' @description Read the gridded livestock of the world 3 dataset.
#' @param subtype Subtype of file to be opened (either Da or Aw)
#' @return Magpie objects
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' readSource("GLW3", subtype = "DA", convert = "onlycorrect")
#' }
#'
#' @import madrat
#' @importFrom raster aggregate raster rasterToPoints
#' @importFrom dplyr left_join
#' @importFrom rgdal GDAL.open

readGLW3 <- function(subtype = "Da") {

    strName <- toolSubtypeSelect(subtype, c(Da = "5_Ct_2010_Da.tif",
                                            Aw = "6_Ct_2010_Aw.tif"))
    # NOTE: GDAL.open is imported from rgdal as it is used in the following
    #       raster command. Making this import explicit is required for renv
    #       so that dependencies are properly detected.
    x <- raster(strName)
    x <- raster::aggregate(x, fact = 6, fun = sum)
    x <- rasterToPoints(x)
    mapping <- toolGetMapping("CountryToCellMapping.rds", where = "mrcommons")
    colnames(x) <- c("lon", "lat", paste0("X5_Ct_2010_", subtype))
    x <- left_join(mapping, x, by = c("lat", "lon"), copy = TRUE)
    x <- as.magpie(x[, c(2, 7)])
    return(x)
  }
