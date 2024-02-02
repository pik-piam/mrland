#' @title calcCroplandTreecover
#'
#' @description Returns area on cropland covered by trees (Mha).

#' @param maginput Whether data should be corrected to align with cropland
#' initialised in MAgPIE.
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return List with a magpie object
#' @author Patrick v. Jeetze
#' @seealso
#' \code{\link{readCopernicus}}
#'
#' @examples
#' \dontrun{
#' calcOutput("calcCroplandTreecover", aggregate = FALSE)
#' }
#'
#' @importFrom mrcommons toolCoord2Isocell
#'
calcCroplandTreecover <- function(maginput = TRUE, cells = "magpiecell") {
  treecover <- readSource("Copernicus", subtype = "CroplandTreecover", convert = "onlycorrect")

  if (maginput) {
    luh <- calcOutput("LUH2v2",
      landuse_types = "magpie", aggregate = FALSE,
      cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
      selectyears = "y2015"
    )
    getYears(luh) <- NULL
    getCells(luh) <- getCells(treecover)

    # cropland treecover area is corrected to make sure that it is not
    # larger than cropland area reported by LUH
    out <- pmax(treecover, luh[, , "crop"])
  } else {
    out <- treecover
  }

  if (cells == "magpiecell") {
    out <- toolCoord2Isocell(out)
  } else if (cells != "lpjcell") {
    stop("Please specify cells argument")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = paste(
      "Cropland area covered by trees"
    ),
    isocountries = FALSE
  ))
}
