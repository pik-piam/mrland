#' @title calcSNVTargetCropland
#'
#' @description Returns cropland area (Mha) that requires relocation in response of
#' maintaining 20% or 50% semi-natural vegetation in farmed landscapes.

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
#' calcOutput("SNVTargetCropland", aggregate = FALSE)
#' }
#'
#' @importFrom mstools toolCoord2Isocell
#'
calcSNVTargetCropland <- function(maginput = TRUE, cells = "magpiecell") {
  targetCropland <- readSource("Copernicus", subtype = "SNVTargetCropland", convert = "onlycorrect")

  if (maginput) {
    luh <- calcOutput("LUH2v2",
      landuse_types = "magpie", aggregate = FALSE,
      cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
      selectyears = "y2015"
    )
    getYears(luh) <- NULL
    getCells(luh) <- getCells(targetCropland)

    # SNV target cropland area is corrected to make sure that it is not
    # larger than cropland area reported by LUH
    for (i in seq(ndata(targetCropland))) {
      targetCropland[, , i] <- pmin(targetCropland[, , i], luh[, , "crop"])
    }
    out <- targetCropland
  } else {
    out <- targetCropland
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
      "Cropland area (Mha) that requires relocation in",
      "response of a share of 20% and 50% semi-natural vegetation",
      "in farmed landscapes"
    ),
    isocountries = FALSE
  ))
}
