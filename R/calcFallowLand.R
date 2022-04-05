#' @title calcFallowLand
#' @description
#' Calculates fallow land on grid cell level,
#' based on physical cropland extend and harvested area output
#' of LPJML io toolbox
#' The formula "fallow land are = max( physical cropland area - harvested cropland area, 0)" is used
#' Due to multicropping harvested cropland area can be greater than non-fallow land area
#' and even greater than physical cropland area.
#' Thus the results can only be considered a rough estimate of fallow land area.
#'
#' @return Magpie object with fallow land in ha
#' @author David Hoetten
#' @seealso
#' \code{\link{readLanduseToolbox}}
#' @examples
#' \dontrun{
#' calcOutput("FallowLand")
#' }
#' @importFrom magclass dimSums mbind
#' @importFrom madrat toolConditionalReplace
#'
calcFallowLand <- function() {

  harvestedArea <- readSource("LanduseToolbox", subtype = "harvestedArea")
  physicalArea <- readSource("LanduseToolbox", subtype = "physicalArea")

  fallowLandRainfed <-  physicalArea[, , 1] - dimSums(harvestedArea[, , 1:17], 3)
  fallowLandIrrigated <- physicalArea[, , 2] - dimSums(harvestedArea[, , 21:37], 3)
  fallowLand <- mbind(fallowLandRainfed, fallowLandIrrigated)

  fallowLand <- toolConditionalReplace(fallowLand, conditions = c("<0"), replaceby = 0)

  return(list(x = fallowLand,
              weight = NULL,
              description = "Fallow land on grid cell level",
              unit = "ha",
              isocountries = FALSE))

}
