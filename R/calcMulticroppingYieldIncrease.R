#' @title calcMulticroppingYieldIncrease
#'
#' @description Calculates yield increase achieved through multiple cropping
#'              (as factor of off season to main season crop yield) under
#'              irrigated and rainfed conditions respectively.
#'              Optionally: return which grid cells are potentially suitable for
#'              multiple cropping activities under rainfed and irrigated conditions.
#'              Calculation is based on grassland gross primary production (GPP)
#'              in the growing period of the respective crop and annual grass GPP.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param minThreshold  Threshold of grass GPP in crop growing period
#'                      and crop yield
#'                      to be considered suitable for multiple cropping.
#'                      Unit of the threshold is gC/m^2.
#'                      Default: 50gC/m^2
#' @param addHarvest    Minimum additional harvest factor (grassGPPannual / grassGPPgrper)
#'                      to be considered suitable for multiple cropping
#' @param fallowFactor  Factor determining yield reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#' @param output        "multicroppingSuitability": returns magpie object
#'                      indicating which grid cells are suitable for multiple cropping,
#'                      "multicroppingYieldIncreaseFactor": factor to determine
#'                      off season yield based on grass GPP
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
#' calcOutput("MulticroppingYieldIncrease", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass getSets mbind getItems new.magpie
#'

calcMulticroppingYieldIncrease <- function(selectyears, lpjml, climatetype, output,
                                           minThreshold = 50, addHarvest = 2, fallowFactor = 0.75,
                                           suitability = "endogenous") {

  ####################
  ### Definitions  ###
  ####################
  # Transformation factor gC/m^2 -> tDM/ha
  yieldTransform    <- 0.01 / 0.45

  ####################
  ### Read in data ###
  ####################
  # grass GPP in the entire year (main + off season) (in tDM/ha)
  grassGPPannual <- setYears(calcOutput("GrassGPP", season = "wholeYear",
                                        lpjml = lpjml[["crop"]], climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)
  # grass GPP in the growing period of LPJmL (main season) (in tDM/ha)
  grassGPPgrper  <- setYears(calcOutput("GrassGPP", season = "mainSeason",
                                        lpjml = lpjml[["crop"]], climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)
  # crop yields in main season provided by LPJmL for LPJmL crop types (in tDM/ha)
  cropYields <- setYears(calcOutput("LPJmL_new", years = selectyears,
                                     version = lpjml[["crop"]], climatetype = climatetype,
                                     subtype = "harvest", stage = "raw", # ToDo: change to smoothed or flexible (see calcYields)
                                     aggregate = FALSE),
                         selectyears)
  cropYields <- cropYields[, , getItems(grassGPPannual, dim = "crop")]
  getSets(cropYields)["d3.1"] <- "crop"
  getSets(cropYields)["d3.2"] <- "irrigation"

  ########################
  ### Data preparation ###
  ########################

  # Initialize cells that are suitable for multiple cropping to 0
  suitMC       <- grassGPPannual
  suitMC[, , ] <- 0

  ####################
  ### Calculations ###
  ####################

  ### Multicropping Mask  ###
  ## Rule 1: Minimum grass yield in main season (growing period of crop)
  minThreshold <- minThreshold * yieldTransform
  rule1        <- grassGPPgrper > minThreshold

  ## Rule 2: Multicropping must lead to at least one full additional harvest
  rule2        <- (grassGPPannual / grassGPPgrper) > 2

  ## Rule 3: Minimum crop yield in main season (growing period of crop)
  rule3        <- cropYields > minThreshold

  ### Cells suitable for multiple cropping given grass GPP & specified rules
  suitMC[rule1 & rule2 & rule3] <- 1

  # Exogenous suitability (provided by GAEZ dataset)
  if (suitability == "exogenous") {
    # Suitability for multicropping under irrigated and rainfed conditions
    tmp                     <- calcOutput("MultipleCroppingZones",
                                          layers = 2, aggregate = FALSE)
    suitMC[, , "irrigated"] <- tmp[, , "irrigated"]
    suitMC[, , "rainfed"]   <- tmp[, , "rainfed"]
  }

  # Perennials not relevant for multiple cropping
  suitMC[, , "sugarcane"]       <- 0

  ### Yield Increase Factor  ###
  # Calculate multiple cropping factor based on annual grass GPP and
  # grass GPP in growing period of crop
  grassGPPoffseason                        <- (grassGPPannual - grassGPPgrper)
  grassGPPoffseason[grassGPPoffseason < 0] <- 0

  increaseFACTOR <- ifelse(grassGPPgrper > 0,
                           grassGPPoffseason / grassGPPgrper,
                           0) * fallowFactor * suitMC

  # Add missing crops (betr, begr, mgrass)
  # [Note: perennials -> not considered for multiple cropping]
  missingCrops <- new.magpie(cells_and_regions = getItems(increaseFACTOR, dim = 1),
                             years = getItems(increaseFACTOR, dim = 2),
                             names = c("betr.irrigated", "betr.rainfed",
                                       "begr.irrigated", "begr.rainfed",
                                       "mgrass.irrigated", "mgrass.rainfed"),
                             fill = 0)
  getSets(missingCrops) <- getSets(increaseFACTOR)
  increaseFACTOR        <- mbind(increaseFACTOR, missingCrops)
  suitMC                <- mbind(suitMC, missingCrops)

  ##############
  ### Checks ###
  ##############

  if (any(is.na(suitMC)) | any(is.na(increaseFACTOR))) {
    stop("calcMulticroppingMask produced NA values")
  }
  if (any(suitMC < 0) | any(increaseFACTOR < 0)) {
    stop("calcMulticroppingMask produced negative values")
  }
  if (any(suitMC != 1 & suitMC != 0)) {
    stop("Problem in calcMulticroppingMask: Value should be 0 or 1!")
  }

  ##############
  ### Return ###
  ##############

  if (output == "multicroppingSuitability") {

    out         <- suitMC
    unit        <- "boolean"
    description <- "Suitability for multicropping under irrigated and rainfed conditions respectively.
                    1 = suitable, 0 = not suitable"

  } else if (output == "multicroppingYieldIncreaseFactor") {

    out         <- increaseFACTOR
    unit        <- "unitless"
    description <- "Factor of yield increase through multiple cropping
                    to be applied on LPJmL crop yield"

  } else {
    stop("Please specify output to be returned by function calcMulticropping:
         multicroppingSuitability or multicroppingYieldIncreaseFactor")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
