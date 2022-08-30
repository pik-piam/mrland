#' @title convertAQUASTAT
#' @description Convert data based on AQUASTAT database (http://www.fao.org/nr/water/aquastat/data/query/index.html?lang=en)
#'
#' @param x MAgPIE object containing AQUASTAT data on country level
#' @param subtype
#' \itemize{
#' \item \code{ConsAgri}:      4454|Conservation agriculture area (1000 ha)
#'                             4454_conservation_agriculture_area_in_1000_ha.csv
#' \item \code{ConsAgriShare}: 4455|Commoditiy Balance LivestockConservation
#'                                  agriculture area as % of arable land area (%)
#'                             4455_conservation_agriculture_area_as_share_of_
#'                                 arable_land_areas.csv)
#' \item \code{rf2irRatio}:    Ratio between rainfed and irrigated yields (%)
#'                             Ratio_between_rainfed_and_irrigated_yields.csv
#' }
#'
#' @return magpie objects with results on contury level
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("AQUASTAT", subtype = "ConsAgri", convert = TRUE)
#' }
#' @import utils

convertAQUASTAT <- function(x, subtype) {

  for (iso3 in getCells(x)) {

    na_years <- getYears(x[iso3, , ], as.integer = TRUE)[which(is.na(x[iso3, , ]))]
    years    <- setdiff(getYears(x[iso3, , ], as.integer = TRUE), na_years)

    if (length(na_years) == length(getYears(x))) {

      if (subtype %in% c("ConsAgri", "ConsAgriShare")) x[iso3, , ] <- 0

    } else {

      if (length(na_years) == length(getYears(x)) - 1) {

        null_year           <- years - 5
        x[iso3, null_year, ]  <- 0
        years               <- c(null_year, years)
        na_years            <- setdiff(na_years, null_year)
      }

      # interpolate years to fill na_years (extrapolate with linear)
      x[iso3, na_years, ]      <- time_interpolate(x[iso3, years, ],
                                                   interpolated_year = na_years,
                                                   extrapolation_type = "linear")

      # removing values below zero (introduced by linear extrapolation)
      x[iso3, , ][x[iso3, , ] < 0] <- 0

      # set all years after last reporting year to last reporting year
      after_years              <- na_years[na_years > tail(years, 1)]
      x[iso3, after_years, ]   <- setYears(x[iso3, tail(years, 1), ], NULL)
    }
  }

  if (subtype %in% c("ConsAgri", "ConsAgriShare")) {
    x <- toolCountryFill(x, fill = 0)

  } else if (subtype %in% c("rf2irRatio")) {

    x   <- toolCountryFill(x, fill = NA)

    ####### MANUAL FIX - NOTE: HAS TO BE CHECKED+FINALIZED

    # Fix manually with best guess nearest neighbor
    x[c("SAU", "KWT", "QAT", "OMN", "ARE"), , ] <- x["YEM", , ]
    x["ESH", , ] <- x["MRT", , ]
    x["TKM", , ] <- x["UZB", , ]
    x["PNG", , ] <- x["IDN", , ]
    x["TWN", , ] <- x["CHN", , ]
    x["EGY", , ] <- x["LBY", , ]
    x["EGY", , ] <- x["LBY", , ]
    x["GNQ", , ] <- x["GAB", , ]
    x["DJI", , ] <- x["ERI", , ]
    x["NCL", , ] <- x["AUS", , ]
    x["GUF", , ] <- x["SUR", , ]
    x["SGP", , ] <- x["MYS", , ]

    # Fix with proxy
    x[is.na(x)]  <- 1.778

    x <- x[, "y1995", ]
  }

  return(x)
}
