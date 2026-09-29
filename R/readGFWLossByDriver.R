#' @title readGFWLossByDriver
#'
#' @description Reads the Global Forest Watch extract downloaded by
#' \code{\link{downloadGFWLossByDriver}}: tree cover loss by country, year and driver, in Mha.
#'
#' @return magpie object, ISO country x year (2001-2025) x driver class, Mha
#' @author Michael Crawford
#' @importFrom utils read.csv head
#' @seealso \code{\link{downloadGFWLossByDriver}}, \code{\link{convertGFWLossByDriver}}
#' @examples
#' \dontrun{
#' a <- readSource("GFWLossByDriver")
#' }

readGFWLossByDriver <- function() {

  # driver classes as named in the data, with GAMS-safe names; the data carries "Unknown" on top of
  # the seven classes of Sims et al. (2025)
  driverClasses <- c("Permanent agriculture"        = "permanent_agriculture",
                     "Hard commodities"             = "hard_commodities",
                     "Shifting cultivation"         = "shifting_cultivation",
                     "Logging"                      = "logging",
                     "Wildfire"                     = "wildfire",
                     "Settlements & Infrastructure" = "settlements_infrastructure",
                     "Other natural disturbances"   = "other_natural_disturbances",
                     "Unknown"                      = "unknown")

  df <- read.csv("loss_by_driver.csv", stringsAsFactors = FALSE)
  missing <- setdiff(c("iso", "year", "threshold", "driver", "loss_ha"), names(df))
  if (length(missing) > 0) {
    stop("GFWLossByDriver: loss_by_driver.csv is missing column(s) ", toString(missing), ".")
  }
  if (anyNA(df$loss_ha) || any(df$loss_ha < 0)) {
    stop("GFWLossByDriver: loss_by_driver.csv carries ", sum(is.na(df$loss_ha)), " missing and ",
         sum(df$loss_ha < 0, na.rm = TRUE), " negative loss_ha values.")
  }
  # one canopy threshold, or the same pixel is counted once per threshold
  if (length(unique(df$threshold)) != 1L) {
    stop("GFWLossByDriver: loss_by_driver.csv mixes canopy thresholds ",
         toString(sort(unique(df$threshold))), "; it must carry exactly one.")
  }

  observed <- sort(unique(df$driver))
  if (!identical(observed, sort(names(driverClasses)))) {
    stop("GFWLossByDriver: the driver classes in the data do not match the class map. ",
         "Only in data: ", toString(setdiff(observed, names(driverClasses))), ". ",
         "Only in map: ", toString(setdiff(names(driverClasses), observed)), ". ",
         "Re-run the preflight and update the class map before trusting any output.")
  }

  # the driver file and the totals file are separate queries of the same table: summed over drivers
  # they must agree, which is what catches a partial or mismatched download
  totals <- read.csv("loss_totals.csv", stringsAsFactors = FALSE)
  byDriver <- stats::aggregate(list(drv = df$loss_ha),
                               by = list(iso = df$iso, year = df$year), FUN = sum)
  both <- merge(byDriver, totals[, c("iso", "year", "loss_ha")], by = c("iso", "year"), all = TRUE)
  if (anyNA(both$drv) || anyNA(both$loss_ha)) {
    stop("GFWLossByDriver: the driver file and the totals file do not cover the same country-years. ",
         sum(is.na(both$drv)), " only in totals, ", sum(is.na(both$loss_ha)),
         " only in the driver file, e.g. ",
         toString(head(paste(both$iso[is.na(both$drv) | is.na(both$loss_ha)],
                             both$year[is.na(both$drv) | is.na(both$loss_ha)]), 5)), ".")
  }
  off <- abs(both$drv - both$loss_ha) > 1e-6 * pmax(both$loss_ha, 1)
  if (any(off)) {
    stop("GFWLossByDriver: driver shares do not sum to the country total for ", sum(off),
         " country-years, e.g. ",
         toString(head(paste0(both$iso[off], " ", both$year[off], " (", signif(both$drv[off], 6),
                              " vs ", signif(both$loss_ha[off], 6), " ha)"), 3)), ".")
  }

  df$driver <- unname(driverClasses[df$driver])
  df$loss <- df$loss_ha / 1e6 # hectares to Mha
  x <- as.magpie(df[, c("iso", "year", "driver", "loss")], spatial = 1, temporal = 2)
  x[is.na(x)] <- 0 # country-year-driver combinations absent from the file carry no loss

  return(magpiesort(x))
}
