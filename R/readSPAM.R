#' @title readSPAM
#'
#' @description Reads SPAM 2010 v2.0 Global Data
#' The items in the technology dimension have the following significations:
#' A =	all technologies together, ie complete crop; I =	irrigated portion of crop;
#' H =	rainfed high inputs portion of crop; L =	rainfed low inputs portion of crop;
#' S =	rainfed subsistence portion of crop; R =	rainfed portion of crop (= A - I, or H + L + S)
#' All data is in ha.
#' @param subtype Type of SPAM data to be readed. Available are "harvestedArea" and "physicalArea".
#'
#' @return magpie object containing output of the toolbox
#'
#' @importFrom magclass as.magpie getNames getSets getSets<- getNames<- getYears<-
#' @importFrom mrcommons toolCoord2Isocell
#' @importFrom terra rast aggregate project
#' @importFrom utils unzip
#' @importFrom dplyr select
#'
#' @author David Hoetten
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' A <- readSource("SPAM", subtype = "harvestedArea")
#' }
#'
readSPAM <- function(subtype) {

    # unzip files
    filenames <- c(harvestedArea = "spam2010v2r0_global_harv_area.geotiff.zip",
                   physicalArea = "spam2010v2r0_global_phys_area.geotiff.zip")

    filename <- toolSubtypeSelect(subtype, filenames)

    dir.create(subtype)
    unzip(filename, exdir =  subtype)

    # create spatraster
    filelist <- list.files(subtype)
    x <- rast(paste0(subtype, "//", filelist))

    # aggregate and reproject raster
    x <- aggregate(x, 6, fun = "sum")

    x <- project(x, rast(res = 0.5))

    # convert to magpie and standardize coordinates
    x <- as.magpie(brick(x))
    x <- toolCoord2Isocell(x, fillMissing = 0)

    # give sensible names that also separate the data dimension
    names <- getNames(x)
    names <- unlist(str_extract_all(unlist(names), "[A-Z]*_[A-Z]*$"))
    names <- gsub("_", ".", names) # The dot separates the data dimension after clean_magpie
    getNames(x) <- names
    x <- clean_magpie(x)

    # name the dimensions
    getSets(x)[4] <- "crop"
    getSets(x)[5] <- "technology"

    # set the year
    getYears(x) <- "y2010"

    return(x)
}
