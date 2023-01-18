#' @title calcGrassGPP
#'
#' @description Calculates gross primary production (GPP) of grassland
#'              under irrigated and rainfed conditions based on LPJmL inputs.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param season        "wholeYear":  grass GPP in the entire year (main + off season)
#'                      "mainSeason": grass GPPP in the crop-specific growing
#'                                    period of LPJmL (main season)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("GrassGPP", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums getItems new.magpie getSets add_dimension
#'

calcGrassGPP <- function(selectyears, lpjml, climatetype, season) {

  # Extract arguments
  cfg <- toolLPJmLVersion(version = lpjml[["crop"]], climatetype = climatetype)
  if (grepl("GSWP3-W5E5", climatetype)) {
    stage       <- "smoothed"
    climatetype <- cfg$baseline_hist
  } else {
    stage <- "harmonized2020"
  }

  ####################
  ### Read in data ###
  ####################

  # monthly irrigated grass GPP
  monthlyIrrigated <- calcOutput("LPJmL_new", subtype = "mgpp_grass_ir",
                              years = selectyears,
                              stage = stage,
                              version = lpjml[["crop"]], climatetype = climatetype,
                              aggregate = FALSE)
  # monthly irrigated grass GPP
  monthlyRainfed <- calcOutput("LPJmL_new", subtype = "mgpp_grass_rf",
                              years = selectyears,
                              stage = stage,
                              version = lpjml[["crop"]], climatetype = climatetype,
                              aggregate = FALSE)

  # irrigated grass GPP in irrigated growing period of crop
  grperIrrigated <- calcOutput("LPJmL_new", subtype = "cft_gpp_grass_ir",
                              years = selectyears,
                              stage = stage,
                              version = lpjml[["crop"]], climatetype = climatetype,
                              aggregate = FALSE)
  # rainfed grass GPP in rainfed growing period of crop
  grperRainfed <- calcOutput("LPJmL_new", subtype = "cft_gpp_grass_rf",
                              years = selectyears,
                              stage = stage,
                              version = lpjml[["crop"]], climatetype = climatetype,
                              aggregate = FALSE)


  ########################
  ### Data preparation ###
  ########################

  # Empty objects to be filled
  grassGPPannual <- grassGPPgrper <- new.magpie(cells_and_regions = getItems(grperIrrigated, dim = 1),
                                                years = getItems(grperIrrigated, dim = 2),
                                                names = getItems(grperIrrigated, dim = 3),
                                                fill = NA)
  # Name dimensions
  getSets(grassGPPannual) <- c("x", "y", "iso", "year", "crop", "irrigation")
  getSets(grassGPPgrper)  <- c("x", "y", "iso", "year", "crop", "irrigation")

  # Extract rainfed grass GPP in rainfed growing period of crop
  grassGPPgrper[, , "rainfed"]   <- grperRainfed[, , "rainfed"]
  # Extract irrigated grass GPP in irrigated growing period of crop
  grassGPPgrper[, , "irrigated"] <- grperIrrigated[, , "irrigated"]


  ####################
  ### Calculations ###
  ####################

  # Monthly grass GPP
  monthlyRainfed   <- add_dimension(monthlyRainfed,
                                    add = "irrigation", nm = "rainfed")
  monthlyIrrigated <- add_dimension(monthlyIrrigated,
                                    add = "irrigation", nm = "irrigated")

  # Calculate annual rainfed grass GPP
  grassGPPannual[, , "rainfed"]   <- dimSums(monthlyRainfed, dim = 3)
  # Calculate annual irrigated grass GPP
  grassGPPannual[, , "irrigated"] <- dimSums(monthlyIrrigated, dim = 3)

  ##############
  ### Return ###
  ##############

  unit        <- "tDM per ha"
  description <- "irrigated and rainfed gross primary production of grass"

  if (season == "mainSeason") {

    out         <- grassGPPgrper
    description <- paste0(description, " in growing season of LPJmL")

  } else if (season == "wholeYear") {

    out         <- grassGPPannual
    description <- paste0(description, " in the entire year")

  } else if (season == "monthly") {

    out <- mbind(monthlyRainfed, monthlyIrrigated)
    getSets(out)["d3.2"] <- "month"
    description <- paste0(description, " per month")

  } else {
    stop("Please specify output to be returned by function calcGrassGPP:
         mainSeason or wholeYear or monthly")
  }


  ##############
  ### Checks ###
  ##############

  if (any(is.na(out))) {
    stop("calcGrassGPP produced NA values")
  }
  if (any(out < 0)) {
    stop("calcGrassGPP produced negative values")
  }
  if (any((grassGPPannual - grassGPPgrper) < 0)) {
    warning("Annual grass GPP < grass GPP in growing period. This may happen
            when using raw rather than smoothed LPJmL inputs due to growing
            periods that can span over two years. It should, however, even out
            when time smoothing is applied.")  # @JENS?
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
