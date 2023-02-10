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
#' @param lpjml         LPJmL version required for respective inputs as single string: "crop" version
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param fallowFactor  Factor determining yield reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#' @param minThreshold  Minimum threshold of grass GPP in crop growing period
#'                      and crop yield to exclude low yielding cells
#'                      Unit of the threshold is gC/m^2.
#'                      Default: 100 gC/m^2
#' @param areaMask      Multicropping area mask to be used
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLanduseToolbox
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific)
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification
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
                                           fallowFactor = 0.75, minThreshold = 100,
                                           areaMask = "potential:endogenous",
                                           crops = "standard") {

  # Function requires lpjml argument in standard format
  if (length(lpjml) == 1) {
    lpjml <- c(natveg = "NULL", crop = lpjml)
  }

  ####################
  ### Definitions  ###
  ####################
  # Transformation factor gC/m^2 -> tDM/ha
  yieldTransform <- 0.01 / 0.45

  # Minimum threshold in tDM/ha
  minThreshold   <- minThreshold * yieldTransform

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
  cropYields  <- calcOutput("YieldsLPJmL", source = lpjml,
                            climatetype = climatetype,
                            years = selectyears,
                            cells = "lpjcell", aggregate = FALSE)

  croplist   <- getItems(grassGPPannual, dim = "crop")
  cropYields <- cropYields[, , croplist]
  getSets(cropYields)["d3.1"] <- "crop"
  getSets(cropYields)["d3.2"] <- "irrigation"

  # Multiple cropping suitability
  if (areaMask == "none") {
    suitMC <- calcOutput("MulticroppingCells",
                          scenario = "potential:exogenous", lpjml = lpjml, climatetype = climatetype,
                          selectyears = selectyears, aggregate = FALSE)[, , croplist]
    # multiple cropping is allowed everywhere
    suitMC[, , ] <- 1
  } else {
    suitMC <- calcOutput("MulticroppingCells", scenario = areaMask,
                          lpjml = lpjml, climatetype = climatetype,
                          selectyears = selectyears, aggregate = FALSE)[, , croplist]

  }

  ####################
  ### Calculations ###
  ####################

  # Exclude cells with grass yields (in growing period of crop) below minimum threshold
  # (Note: for numerical reasons)
  rule1 <- grassGPPgrper > minThreshold

  # Exclude low yielding cells (minimum crop yield in main season below minimum threshold)
  # (Note: to account for non-matching growing periods)
  rule2 <- cropYields > minThreshold

  ### Yield Increase Factor  ###
  # Calculate multiple cropping factor based on annual grass GPP and
  # grass GPP in growing period of crop
  grassGPPoffseason                        <- (grassGPPannual - grassGPPgrper)
  grassGPPoffseason[grassGPPoffseason < 0] <- 0

  increaseFACTOR <- ifelse(rule1 & rule2,
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
  getSets(proxyIncrease) <- getSets(increaseFACTOR)
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
