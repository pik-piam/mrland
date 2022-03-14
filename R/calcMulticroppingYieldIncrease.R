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
#' @param minThreshold  Threshold of grass GPP in crop growing period to be 
#'                      considered suitable for multiple cropping.
#'                      Unit of the threshold is gC/m^2.
#'                      Default: 100gC/m^2
#' @param addHarvest    Minimum additional harvest factor (grassGPPannual / grassGPPgrper)
#'                      to be considered suitable for multiple cropping
#' @param fallowFactor  Factor determining yield reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#' @param output        "multicroppingSuitability": returns magpie object 
#'                      indicating which grid cells are suitable for multiple cropping, 
#'                      "multicroppingYieldIncreaseFactor": factor to determine 
#'                      off season yield based on grass GPP
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
                                           minThreshold = 100, addHarvest = 2, fallowFactor = 0.75) {
  
  ####################
  ### Definitions  ###
  ####################
  # Transformation factor gC/m^2 -> tDM/ha
  yieldTransform    <- 0.01 / 0.45
  
  ####################
  ### Read in data ###
  ####################
  # grass GPP in the entire year (main + off season) (in tDM/ha)
  grassGPPannual <- calcOutput("GrassGPP", season = "wholeYear",
                               lpjml = lpjml, climatetype = climatetype, 
                               selectyears = selectyears, aggregate = FALSE) 
  # grass GPP in the growing period of LPJmL (main season) (in tDM/ha)
  grassGPPgrper  <- calcOutput("GrassGPP", season = "mainSeason",
                              lpjml = lpjml, climatetype = climatetype, 
                              selectyears = selectyears, aggregate = FALSE) 
  
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
  
  ### Cells suitable for multiple cropping given grass GPP & specified rules
  suitMC[rule1 & rule2]   <- 1
  
  # Perennials not relevant for multiple cropping 
  suitMC[, , "sugarcane"] <- 0
  
  ### Yield Increase Factor  ###
  # Calculate multiple cropping factor based on annual grass GPP and 
  # grass GPP in growing period of crop
  grassGPPoffseason                        <- (grassGPPannual - grassGPPgrper)
  grassGPPoffseason[grassGPPoffseason < 0] <- 0

  increaseFACTOR <- ifelse(grassGPPgrper > 0, grassGPPoffseason / grassGPPgrper, 
                           0) * fallowFactor * suitMC
  
  # Add missing crops (betr, begr, mgrass) [Note: perennials]
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
    description <- "Suitability for multicropping under irrigated and rainfed conditions respectively. 1=suitable, 0=not suitable"
    
  } else if (output == "multicroppingYieldIncreaseFactor") {
    
    out         <- increaseFACTOR
    unit        <- "unitless"
    description <- "Factor of yield increase through multiple cropping to be applied on LPJmL crop yield"
    
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
