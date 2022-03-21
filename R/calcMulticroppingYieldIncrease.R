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
#' @param fallowFactor  Factor determining yield reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#' @param suitability   "endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set
#' @param crops         standard: default crops,
#'                      proxy: proxy crops for LPJmL to MAgPIE mapping and treatment of perennials
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

calcMulticroppingYieldIncrease <- function(selectyears, lpjml, climatetype,
                                           fallowFactor = 0.75,
                                           suitability = "endogenous",
                                           crops = "standard") {

  ####################
  ### Read in data ###
  ####################
  # grass GPP in the entire year (main + off season) (in tDM/ha)
  grassGPPannual <- setYears(calcOutput("GrassGPP", season = "wholeYear",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)
  # grass GPP in the growing period of LPJmL (main season) (in tDM/ha)
  grassGPPgrper  <- setYears(calcOutput("GrassGPP", season = "mainSeason",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)
  # crop yields in main season provided by LPJmL for LPJmL crop types (in tDM/ha)
  cropYields <- setYears(calcOutput("LPJmL_new", years = selectyears,
                                     version = lpjml, climatetype = climatetype,
                                    subtype = "harvest", stage = "smoothed", #@KRISTINE: To confirm: Should this be harmonized or smoothed or flexible? 
                                    aggregate = FALSE),
                         selectyears)
  croplist   <- getItems(grassGPPannual, dim = "crop")
  cropYields <- cropYields[, , croplist]
  getSets(cropYields)["d3.1"] <- "crop"
  getSets(cropYields)["d3.2"] <- "irrigation"

  # Multiple cropping suitability
  suitMC <- calcOutput("MulticroppingSuitability", suitability = suitability,
                       lpjml = lpjml, climatetype = climatetype,
                       selectyears = selectyears, aggregate = FALSE)[, , croplist]

  ####################
  ### Calculations ###
  ####################

  ### Yield Increase Factor  ###
  # Calculate multiple cropping factor based on annual grass GPP and
  # grass GPP in growing period of crop
  grassGPPoffseason                        <- (grassGPPannual - grassGPPgrper)
  grassGPPoffseason[grassGPPoffseason < 0] <- 0

  increaseFACTOR <- ifelse(grassGPPgrper > 0,
                           grassGPPoffseason / grassGPPgrper,
                           0) * fallowFactor * suitMC

  # Add missing crops (betr, begr, mgrass)
  # [Note: grown throughout the whole year] ---> set to 0
  missingCrops <- new.magpie(cells_and_regions = getItems(increaseFACTOR, dim = 1),
                             years = getItems(increaseFACTOR, dim = 2),
                             names = c("betr.irrigated", "betr.rainfed",
                                       "begr.irrigated", "begr.rainfed",
                                       "mgrass.irrigated", "mgrass.rainfed"),
                             fill = 0)
  getSets(missingCrops) <- getSets(increaseFACTOR)
  increaseFACTOR        <- mbind(increaseFACTOR, missingCrops)


  # Some MAgPIE perennial crops or crops that are grown throughout the whole year
  # are proxied by an LPJmL crop that is grown with seasonality.
  # For these, full year yields are calculated in all locations. This implies
  # (a) no masking of multicropping suitability (suitable everywhere)
  # (b) no fallow factor applied
  proxyIncrease <- new.magpie(cells_and_regions = getItems(increaseFACTOR, dim = 1),
                              years = getItems(increaseFACTOR, dim = 2),
                              names = c("groundnut.irrigated", "groundnut.rainfed",
                                        "maize.irrigated", "maize.rainfed"),
                              fill = NA)
  proxyIncrease[, , "groundnut"] <- ifelse(grassGPPgrper[, , "groundnut"] > 0,
                                          grassGPPoffseason[, , "groundnut"] / grassGPPgrper[, , "groundnut"],
                                        0)
  proxyIncrease[, , "maize"]     <- ifelse(grassGPPgrper[, , "maize"] > 0,
                                          grassGPPoffseason[, , "maize"] / grassGPPgrper[, , "maize"],
                                         0)
  getSets(proxyIncrease)       <- getSets(increaseFACTOR)
  # @KRISTINE, JENS: Check whether that makes sense

  ##############
  ### Checks ###
  ##############

  if (any(is.na(increaseFACTOR))) {
    stop("calcMulticroppingMask produced NA values")
  }
  if (any(increaseFACTOR < 0)) {
    stop("calcMulticroppingMask produced negative values")
  }

  ##############
  ### Return ###
  ##############

  if (crops == "proxy") {
    out <- proxyIncrease
  } else {
    out <- increaseFACTOR
  }

  unit        <- "unitless"
  description <- "Factor of yield increase through multiple cropping
                  to be applied on LPJmL crop yield"

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
