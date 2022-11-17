#' @title calcMulticroppingSuitability
#'
#' @description Calculates which grid cells are potentially suitable for
#'              multiple cropping activities under rainfed and irrigated conditions.
#'              Calculation is based on grassland gross primary production (GPP)
#'              in the growing period of the respective crop and annual grass GPP
#'              as well as the specific crop yield in the growing period.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param minThreshold  Threshold of monthly grass GPP to be classified as
#'                      growing period month
#'                      Unit of the threshold is gC/m^2.
#'                      Default: 100gC/m^2
#' @param suitability   "endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingSuitability", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass setYears getSets mbind getItems new.magpie
#'

calcMulticroppingSuitability <- function(selectyears, lpjml, climatetype,
                                         minThreshold = 100, suitability = "endogenous") {
  ####################
  ### Read in data ###
  ####################
  # grass GPP in the entire year (main + off season) (in tDM/ha)
  grassGPPannual <- setYears(calcOutput("GrassGPP", season = "wholeYear",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)

  ########################
  ### Data preparation ###
  ########################

  # Initialize cells that are suitable for multiple cropping to 0
  suitMC       <- grassGPPannual
  suitMC[, , ] <- 0

  # Choose how multiple cropping suitability is determined
  if (suitability == "endogenous") {
    ####################
    ### Definitions  ###
    ####################
    # Transformation factor gC/m^2 -> tDM/ha
    yieldTransform <- 0.01 / 0.45

    ####################
    ### Read in data ###
    ####################
    # grass GPP in the growing period of LPJmL (main season) (in tDM/ha)
    grassGPPgrper <- setYears(calcOutput("GrassGPP", season = "mainSeason",
                                          lpjml = lpjml, climatetype = climatetype,
                                          selectyears = selectyears, aggregate = FALSE),
                               selectyears)
    # monthly grass GPP (in tDM/ha)
    grassGPPmonth <- setYears(calcOutput("GrassGPP", season = "monthly",
                                         lpjml = lpjml, climatetype = climatetype,
                                         selectyears = selectyears, aggregate = FALSE),
                              selectyears)

    ####################
    ### Calculations ###
    ####################

    # Calculate length of growing period
    lgp       <- grassGPPmonth
    lgp[, , ] <- 0
    # Classification as growing period month when monthly grass GPP > 100gC/m^2
    thresholdLGP <- minThreshold * yieldTransform
    lgp[grassGPPmonth >= thresholdLGP] <- 1
    lgp <- dimSums(lgp, dim = "month")

    ### Multicropping Mask  ###
    ## Rule 1: Minimum length of growing period of 8 months
    rule1 <- lgp > 8

    ## Rule 2: Multicropping must lead to at least one full additional harvest
    rule2 <- (grassGPPannual / grassGPPgrper) > 2

    ### Cells suitable for multiple cropping given grass GPP & specified rules
    suitMC[rule1 & rule2] <- 1

  } else if (suitability == "exogenous") {
    ####################
    ### Read in data ###
    ####################
    suitMC[, , ] <- calcOutput("MultipleCroppingZones", layers = 2, aggregate = FALSE)

  } else {
    stop("Please select whether endogenously calculated multiple cropping suitability
         mask (endogenous) should be selected or whether
         GAEZ Multiple Cropping Zones data set should be used (exogenous)")
  }

  # For perennials that are grown throughout the whole year,
  # multicropping yield is equal to single cropping yield
  suitMC[, , "sugarcane"] <- 0

  # Add missing crops (betr, begr, mgrass)
  # [Note: grown throughout the whole year -> multicropping yield = single cropping]
  missingCrops <- new.magpie(cells_and_regions = getItems(suitMC, dim = 1),
                             years = getItems(suitMC, dim = 2),
                             names = c("betr.irrigated", "betr.rainfed",
                                       "begr.irrigated", "begr.rainfed",
                                       "mgrass.irrigated", "mgrass.rainfed"),
                             fill = 0)
  suitMC       <- mbind(suitMC, missingCrops)

  ##############
  ### Checks ###
  ##############

  if (any(is.na(suitMC))) {
    stop("calcMulticroppingSuitability produced NA values")
  }
  if (any(suitMC < 0)) {
    stop("calcMulticroppingSuitability produced negative values")
  }
  if (any(suitMC != 1 & suitMC != 0)) {
    stop("Problem in calcMulticroppingSuitability: Value should be 0 or 1!")
  }

  ##############
  ### Return ###
  ##############
  out         <- suitMC
  unit        <- "boolean"
  description <- "Suitability for multicropping of different crops under
                  irrigated and rainfed conditions respectively.
                  1 = suitable, 0 = not suitable"

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
