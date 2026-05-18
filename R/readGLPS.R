#' @title readGLPS
#' @description Reads Global Livestock Production System (GLPS) data for
#'   reference year 2010. Three animal groups are available:
#'   \itemize{
#'     \item Chickens: backyard (extensive) and intensive management –
#'       continuous density raster (heads/pixel)
#'     \item Pigs: backyard (extensive), semi-intensive, and industrial –
#'       continuous density raster (heads/pixel)
#'     \item Ruminants: categorical production system map (LPS class code
#'       per pixel; aggregated to 0.5 degree by modal value)
#'   }
#' @param subtype Animal group and management system for 2010. Available
#'   options:
#'   \itemize{
#'     \item Chicken – \code{Ch_Ext_2010} (backyard/extensive),
#'       \code{Ch_Int_2010} (intensive)
#'     \item Pig – \code{Pg_Ext_2010} (backyard/extensive),
#'       \code{Pg_Int_2010} (semi-intensive),
#'       \code{Pg_Ind_2010} (industrial/intensive)
#'     \item Ruminant – \code{Ruminant_2000} (categorical LPS raster ca. 2000)
#'   }
#' @return A gridded magpie object. Monogastric subtypes: heads/pixel.
#'   Ruminant_2000: categorical LPS class code per pixel.
#' @author Bin Lin
#' @examples
#' \dontrun{
#' readSource("GLPS", subtype = "Ch_Ext_2010", convert = FALSE)
#' readSource("GLPS", subtype = "Ruminant_2000", convert = FALSE)
#' }
#' @importFrom terra rast aggregate modal
#' @importFrom madrat toolSubtypeSelect
#' @importFrom magclass as.magpie getYears<- getNames<-

readGLPS <- function(subtype = "Ch_Ext_2010") {

  monogastricFiles <- c(
    Ch_Ext_2010 = "06_ChExt_2010_Da.tif",
    Ch_Int_2010 = "07_ChInt_2010_Da.tif",
    Pg_Ext_2010 = "8_PgExt_2010_Da.tif",
    Pg_Int_2010 = "9_PgInt_2010_Da.tif",
    Pg_Ind_2010 = "10_PgInd_2010_Da.tif"
  )

  monogastricSubtypes <- names(monogastricFiles)

  year <- paste0("y", substr(subtype, nchar(subtype) - 3, nchar(subtype)))

  if (subtype %in% monogastricSubtypes) {
    file <- toolSubtypeSelect(subtype, monogastricFiles)
    x <- rast(file)
    x <- aggregate(x, fact = 6, fun = sum, na.rm = TRUE)
    x <- as.magpie(x)
    getYears(x) <- year
    getNames(x) <- subtype
    attr(x, "unit") <- "heads/pixel"
    return(x)
  }

  if (subtype == "Ruminant_2000") {
    gisFile <- list.files(pattern = "\\.(tif|img|asc)$", ignore.case = TRUE)[1]
    if (is.na(gisFile)) stop("No raster file found for Ruminant_2000. Run downloadSource first.")
    x <- rast(gisFile)
    x <- aggregate(x, fact = 6, fun = modal, na.rm = TRUE)
    x <- as.magpie(x)
    getYears(x) <- year
    getNames(x) <- subtype
    attr(x, "unit") <- "categorical (LPS class code)"
    return(x)
  }

  allSubtypes <- c(monogastricSubtypes, "Ruminant_2000")
  stop("Unknown subtype '", subtype, "'. Available: ",
       paste(allSubtypes, collapse = ", "))
}
