#' @title calcMulticroppingIntensity
#'
#' @description Returns cropping intensity according to Landuse Toolbox
#'              given the chosen scenario
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param scenario      "total": currently multicropped areas calculated from total harvested areas
#'                               and total physical areas per cell from readLanduseToolbox
#'                      "crop" (crop-specific), "irrigation" (irrigation-specific),
#'                      "irrig_crop" (crop- and irrigation-specific)
#' @param sectoral      "kcr" MAgPIE crop types, and "lpj" LPJmL crop types
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingIntensity", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput readSource toolGetMapping
#' @importFrom magclass dimSums
#'

calcMulticroppingIntensity <- function(scenario, selectyears,
                                       lpjml, climatetype, sectoral = "lpj") {

  # areas where multicropping takes place currently (crop- and irrigation-specific)
  phys <- calcOutput("CropareaToolbox", physical = TRUE, sectoral = sectoral,
                      cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                      selectyears = selectyears, aggregate = FALSE)
  harv <- calcOutput("CropareaToolbox", physical = FALSE, sectoral = sectoral,
                      cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                      selectyears = selectyears, aggregate = FALSE)
  # keep for dimensionality
  phys[, , ] <- NA
  harv[, , ] <- NA

  if (scenario == "total") {

    # total actual multicropping area
    tempPhys <- dimSums(calcOutput("CropareaToolbox", physical = TRUE, sectoral = sectoral,
                                    cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                                    selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")
    tempHarv <- dimSums(calcOutput("CropareaToolbox", physical = FALSE, sectoral = sectoral,
                                    cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                                    selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "irrig") {

    # total actual multicropping area (irrigation-specific)
    tempPhys <- dimSums(calcOutput("CropareaToolbox", physical = TRUE, sectoral = sectoral,
                                    cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                                    selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")
    tempHarv <- dimSums(calcOutput("CropareaToolbox", physical = FALSE, sectoral = sectoral,
                                    cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                                    selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "crop") {

    # areas where multicropping takes place currently (crop-specific)
    tempPhys <- calcOutput("CropareaToolbox", physical = TRUE, sectoral = sectoral,
                            cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                            selectyears = selectyears, aggregate = FALSE)
    tempHarv <- calcOutput("CropareaToolbox", physical = FALSE, sectoral = sectoral,
                            cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                            selectyears = selectyears, aggregate = FALSE)

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "irrig_crop") {

    # areas where multicropping takes place currently (crop- and irrigation-specific)
    phys <- calcOutput("CropareaToolbox", physical = TRUE, sectoral = sectoral,
                        cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                        selectyears = selectyears, aggregate = FALSE)
    harv <- calcOutput("CropareaToolbox", physical = FALSE, sectoral = sectoral,
                        cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                        selectyears = selectyears, aggregate = FALSE)

  } else {
    stop("Please select whether total, irrigation-specific (irrig), crop-specific (crop),
    or irrigation- and crop-specific (irrig_crop) cropping intensity shall be
    returned in the scenario argument of calcMulticroppingIntensity")
  }

  # cropping intensity factor
  currMC <- ifelse(phys > 0, harv / phys, NA)

  # corrections
  currMC[currMC > 2]    <- 2
  currMC[is.na(currMC)] <- 1

  ##############
  ### Checks ###
  ##############

  if (any(is.na(currMC))) {
    stop("calcMulticroppingIntensity produced NA values")
  }
  if (any(currMC > 2 || currMC < 1)) {
    stop("Problem in calcMulticroppingIntensity:
          Value should be between 1 and 2!")
  }

  ##############
  ### Return ###
  ##############
  out         <- currMC
  unit        <- "factor"
  description <- "Cropping Intensitiy of different crops under
                  irrigated and rainfed conditions respectively"

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
