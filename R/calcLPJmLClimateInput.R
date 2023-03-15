#' @title calcLPJmLClimateInput
#' @description Handle LPJmL climate input data and its time behaviour
#'              (smoothing and harmonizing to baseline)
#'
#' @param climatetype Switch between different climate scenario
#' @param variable Switch between different climate inputs and temporal resolution
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param lpjmlVersion LPJmL Version hand over
#'
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("LPJmLClimateInput",
#'            climatetype = "IPSL-CM6A-LR:ssp126",
#'            variable = "temperature:annual_mean")
#' }
#'
#' @importFrom madrat toolSplitSubtype toolTimeAverage
#' @importFrom magclass getNames
#' @importFrom magpiesets findset
#' @importFrom mstools toolHoldConstant
#' @importFrom SPEI thornthwaite
#'

calcLPJmLClimateInput <- function(climatetype = "IPSL-CM6A-LR:ssp126",
                                  variable = "temperature:annual_mean",
                                  stage = "harmonized2020",
                                  lpjmlVersion = NULL) { #Needed here for consistency with LPJmL?

  # Create settings for LPJmL/GCM from version and climatetype argument
  cfg <- toolClimateInputVersion(lpjmlVersion = lpjmlVersion,
                                 climatetype = climatetype)
  var <- toolSplitSubtype(variable, list(type = NULL, timeres = NULL))

  if (stage %in% c("raw", "smoothed")) {

    ########## PLUG HIST + FUTURE ##########


    if (!grepl("historical", climatetype)) {

      .subtypeScen <- paste(cfg$versionScen, cfg$climatetype, var$type, sep = ":")
      .subtypeHist <- gsub("ssp[0-9]{3}", "historical", .subtypeScen)

      # For climate scenarios historical GCM data has to be read in from a different file
      x <- mbind(readSource("GCMClimate", subtype = .subtypeHist,
                            subset = var$timeres, convert = "onlycorrect"),
                 readSource("GCMClimate", subtype = .subtypeScen,
                            subset = var$timeres, convert = "onlycorrect"))
      years <- getYears(x, as.integer = TRUE)
      x     <- x[, years[years >= 1971], ]

    } else {

      .subtypeHist <- paste(cfg$versionHist, cfg$climatetype, var$type, sep = ":")
      x     <- readSource("LPJmL_new", subtype = .subtypeHist, convert = FALSE)
      years <- getYears(x, as.integer = TRUE)
      x     <- x[, years[years >= 1931], ]
    }
    ########## PLUG HIST + FUTURE ##########

    if (stage == "smoothed") {
      out <- toolSmooth(x)
    } else {
      out <- x
    }

  } else if (grepl("harmonized", stage)) {

    harmStyle <- switch(var$type,
                        "temperature"   = "additive",
                        "precipitation" = "limited",
                        "longWaveNet"   = stop(paste0("No harmonization available for: ", x$variable)),
                        "shortWave"     = stop(paste0("No harmonization available for: ", x$variable)),
                        "wetDaysMonth"  = stop(paste0("No harmonization available for: ", x$variable)))

    if (stage == "harmonized") {
      # read in historical data for subtype
      baseline <- calcOutput("LPJmLClimateInput", climatetype = cfg$baselineHist,
                             variable = variable, stage = "smoothed", aggregate = FALSE)
      x        <- calcOutput("LPJmLClimateInput", climatetype = cfg$climatetype,
                             variable = variable, stage = "smoothed", aggregate = FALSE)
      out <- toolHarmonize2Baseline(x, baseline, ref_year = cfg$refYearHist, method = harmStyle)

    } else if (stage == "harmonized2020") {
      # read in historical data for subtype
      baseline2020 <- calcOutput("LPJmLClimateInput", climatetype = cfg$baselineGcm,
                                 variable = variable, stage = "harmonized", aggregate = FALSE)

      if (cfg$climatetype    == cfg$baselineGcm) {

        out <- baseline2020

      } else {

        x   <- calcOutput("LPJmLClimateInput", climatetype = cfg$climatetype,
                               variable = variable, stage = "smoothed", aggregate = FALSE)
        out <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfg$refYearGcm, method = harmStyle)
      }

    } else {
      stop("Stage argument not supported!")
    }
  } else {
    stop("Stage argument not supported!")
  }

  unit <- switch(var$type,
                 "temperature"   = "Degree Celcius",
                 "precipitation" = "mm/day",
                 "longWaveNet"   = "watt per m2",
                 "shortWave"     = "watt per m2",
                 "wetDaysMonth"  = "number of rainy days")

  description <- switch(var$type,
                        "temperature"   = paste0("Average ", x$timeres, " air temperature"),
                        "precipitation" = paste0("Average ", x$timeres, " precipitation"),
                        "longWaveNet"   = "Long wave radiation",
                        "ShortWave"     = "Short wave radiation",
                        "wetDaysMonth"  = "number of rainy days")

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
