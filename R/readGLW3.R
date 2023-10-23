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

readGLW3 <- function(subtype = "Da") {

    strName <- toolSubtypeSelect(subtype, c(Da = "5_Ct_2010_Da.tif",
                                            Aw = "6_Ct_2010_Aw.tif"))
    x <- raster(strName)
    x <- raster::aggregate(x, fact = 6, fun = sum, na.rm = TRUE)
    x <- rasterToPoints(x)

    colnames(x) <- c("lon", "lat", paste0("X5_Ct_2010_", subtype))

    # mapping with coordinates of 67420 grid cells
    mapping          <- toolGetMappingCoord2Country()
    mapping$coordiso <- paste(mapping$coords, mapping$iso, sep = ".")
    mapping$lat      <- as.numeric(gsub("p", ".", gsub(".*\\.", "", mapping$coords)))
    mapping$lon      <- as.numeric(gsub("p", ".", gsub("\\..*", "", mapping$coords)))

    # reduce number of cells and transform to magpie object
    x <- left_join(mapping, x, by = c("lat", "lon"), copy = TRUE)
    x <- as.magpie(x[, c(3, 6)], spatial = 1)
    getItems(x, dim = 1, raw = TRUE) <- mapping$coordiso
    getSets(x) <- c("x", "y", "iso", "year", "data")

    return(x)
  }
