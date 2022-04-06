#' @title calcCropareaToolbox
#' @description This function uses the data from the LPJmL io Toolbox
#' to calculate cropareas in various formats.
#' !!This is still a WIP!!
#'
#' @param physical   if TRUE the sum over all crops agrees with the cropland area per country
#' (not yet completly true, WIP)
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param irrigation If true: cellular areas are returned separated
#'                   into irrigated and rainfed
#'
#' @return Magpie object with cropareas
#'
#' @author David Hötten
#'
calcCropareaToolbox <- function(physical = TRUE,
                                cells = "magpiecell",
                                irrigation = FALSE) {

  harvestedArea <- readSource("LanduseToolbox", subtype = "harvestedArea")

  allCrops <- getItems(harvestedArea, split = TRUE)[[3]]$crop
  allRealCrops <- allCrops[!(allCrops %in% c("pasture", "begr", "betr"))]
  harvestedArea <- harvestedArea[, , allRealCrops]

  if (!physical) {
    output <- harvestedArea
  } else {
    physicalArea <- readSource("LanduseToolbox", subtype = "physicalArea")

    # for the following crops we know that no multicropping is happening, so physical area = harvested area
    perenials <- c("sugr_cane", "oilpalm")
    nonper <- allRealCrops[!(allCrops %in% perenials)] # nonper means nonperenials

    perenialHarvestedA <-  dimSums(harvestedArea[, , perenials], c("crop"))
    nonperHarvestedA <- dimSums(harvestedArea[, , nonper], c("crop"))

    # check how  much physical area is remaining for the nonperenials after substracting the perenial physical area
    nonperPhysicalA <- physicalArea - perenialHarvestedA # we can do that since for perenial physical=harvested

    # calculate a factor by which the nonperennials should be scaled down so the sum matches nonperPhysicalA
    scaling <- (nonperPhysicalA / nonperHarvestedA) * (nonperHarvestedA > 0) * (nonperPhysicalA > 0)

    scaling[is.na(scaling)] <- 0
    # if nonperennial harvested area is smaller than it's physical counterpart don't cange it
    scaling[scaling > 1] <- 1

    physicalAreaCrop <- harvestedArea
    physicalAreaCrop[, , nonper] <- harvestedArea[, , nonper] * scaling

    output <- physicalAreaCrop
  }

  if (!irrigation) {
    output <- dimSums(output, "irrigation")
  }

  if (cells == "magpiecell") {
    output <- toolCoord2Isocell(output)
  }

  return(list(x = output,
              weight = NULL,
              description = "Croparea for different croptypes",
              unit = "mha",
              isocountries = FALSE))
}
