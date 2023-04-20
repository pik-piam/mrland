#' @title calcMulticroppingSuitability
#'
#' @description Calculates which grid cells are potentially suitable for
#'              multiple cropping activities under rainfed and irrigated conditions.
#'              Calculation is based on grassland gross primary production (GPP)
#'              in the growing period of the respective crop and annual grass GPP
#'              as well as the specific crop yield in the growing period.
#'
#' @param selectyears    Years to be returned
#' @param lpjml          LPJmL version required for respective inputs: natveg or crop
#' @param climatetype    Switch between different climate scenarios or
#'                       historical baseline "GSWP3-W5E5:historical"
#' @param temperatureGCM If !NULL:
#'                       Additional crop-specific temperature rule
#'                       GCM Climate data for temperature.
#'                       Specify data source, climate scenario, ssp, years
#'                       in the following format:
#'                       "ISIMIP3b:IPSL-CM6A-LR:ssp126:1850-2100"
#' @param minThreshold   Threshold of monthly grass GPP to be classified as
#'                       growing period month
#'                       Unit of the threshold is gC/m^2.
#'                       Default: 100gC/m^2
#' @param suitability    "endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                       "exogenous": suitability for multiple cropping given by
#'                                    GAEZ data set
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
                                         temperatureGCM = NULL,
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
  croplist <- getItems(suitMC, dim = "crop")

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
    rule1 <- rule2 <- rule3 <- suitMC
    rule1[, , ] <- rule2[, , ] <- rule3[, , ] <- NA

    if (!is.null(temperatureGCM)) {

      # Crop-specific max/min photosynthesis temperatures
      temp <- calcOutput("PhotosynthesisTemperature", aggregate = FALSE)
      # FELI: Make temperature GCM flexible
      meanMonthTemp <- collapseNames(calcOutput("LPJmLClimateInput",
                                                subtype = paste0(temperatureGCM, ":tas:monthly_mean"),
                                                smooth = 0, # KRISTINE: Or should I smooth them?
                                                cells = "lpjcell", aggregate = FALSE))[, selectyears, ]
      monthTempLimit <- meanMonthTemp
      monthTempLimit <- add_dimension(monthTempLimit, dim = 3.1,
                                      add = "crop", nm = getItems(temp, dim = "crop"))

      minTemp <- (monthTempLimit >= collapseNames(temp[, , "min"]))
      maxTemp <- (monthTempLimit <= collapseNames(temp[, , "max"]))

      monthTempLimit[, , ] <- 0
      monthTempLimit[minTemp & maxTemp] <- 1
      monthTempLimit <- dimSums(monthTempLimit, dim = "month")
      monthTempLimit <- monthTempLimit[, , croplist]

      ## Rule 1: Temperature range reached in at least 8 months
      rule1[, , croplist] <- monthTempLimit > 7
      ### JENS: Should be included or not? Should be 7 or 8? should be monthly or daily?

    } else {

      ## Rule 1: Temperature rule does not apply here (set to always true)
      rule1[, , ] <- 1

    }

    ## Rule 2: Minimum length of growing period of 9 months
    rule2[, , "rainfed"]   <- (lgp > 8)[, , "rainfed"]
    rule2[, , "irrigated"] <- (lgp > 8)[, , "irrigated"]

    ## Rule 3: Multicropping must lead to at least one full additional harvest
    rule3 <- (grassGPPannual / grassGPPgrper) > 2

    ### Cells suitable for multiple cropping given specified rules
    suitMC[rule1 & rule2 & rule3] <- 1

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
  suitMC[, , "trro"]      <- 0

  # Add missing crops (betr, begr, mgrass)
  # [Note: grown throughout the whole year -> multicropping yield = single cropping]
  missingCrops <- new.magpie(cells_and_regions = getItems(suitMC, dim = 1),
                             years = getItems(suitMC, dim = 2),
                             names = c("betr.irrigated", "betr.rainfed",
                                       "begr.irrigated", "begr.rainfed",
                                       "mgrass.irrigated", "mgrass.rainfed"),
                             fill = 0)
  suitMC       <- mbind(suitMC, missingCrops)

  # If multiple cropping is possible under rainfed conditions,
  # it's also possible under irrigated conditions
  rfMC <- collapseNames(suitMC[, , "rainfed"])
  suitMC[, , "irrigated"][rfMC == 1] <- 1

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
