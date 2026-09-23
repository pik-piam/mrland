#' @title readGFWLossByDriver
#'
#' @description Reads the Global Forest Watch extract downloaded by
#' \code{\link{downloadGFWLossByDriver}}: tree cover loss by country, year and driver
#' (\code{subtype = "loss"}, default) or tree cover extent in 2000 at the same canopy threshold
#' (\code{subtype = "extent"}). Both in Mha.
#'
#' @param subtype \code{"loss"} (default) or \code{"extent"}
#' @return magpie object; loss by ISO country x year (2001-2025) x driver class, extent by ISO
#' country for y2000. Mha.
#' @author Michael Crawford
#' @importFrom magclass as.magpie magpiesort
#' @importFrom utils read.csv head
#' @seealso \code{\link{downloadGFWLossByDriver}}, \code{\link{convertGFWLossByDriver}}
#' @examples
#' \dontrun{
#' a <- readSource("GFWLossByDriver")
#' }

readGFWLossByDriver <- function(subtype = "loss") {

  if (!subtype %in% c("loss", "extent")) {
    stop("readGFWLossByDriver: unknown subtype '", subtype, "'. Use \"loss\" or \"extent\".")
  }

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

  # required columns present, value column finite and non-negative, exactly one canopy threshold
  checkFile <- function(d, cols, value, file) {
    missing <- setdiff(cols, names(d))
    if (length(missing) > 0) {
      stop("GFWLossByDriver: ", file, " is missing column(s) ", toString(missing), ".")
    }
    if (anyNA(d[[value]]) || any(d[[value]] < 0)) {
      stop("GFWLossByDriver: ", file, " carries ", sum(is.na(d[[value]])), " missing and ",
           sum(d[[value]] < 0, na.rm = TRUE), " negative ", value, " values.")
    }
    # the share MAgPIE consumes is loss / extent, so one threshold per file and the same one in
    # both; that the value is the pinned one is checked where it is pinned, in the download
    if (length(unique(d$threshold)) != 1L) {
      stop("GFWLossByDriver: ", file, " mixes canopy thresholds ",
           toString(sort(unique(d$threshold))), "; it must carry exactly one.")
    }
    invisible(d)
  }

  df <- read.csv("loss_by_driver.csv", stringsAsFactors = FALSE)
  checkFile(df, c("iso", "year", "threshold", "driver", "loss_ha"), "loss_ha", "loss_by_driver.csv")

  if (subtype == "extent") {
    ext <- read.csv("extent_2000.csv", stringsAsFactors = FALSE)
    checkFile(ext, c("iso", "threshold", "extent_ha"), "extent_ha", "extent_2000.csv")

    if (!identical(unique(ext$threshold), unique(df$threshold))) {
      stop("GFWLossByDriver: extent_2000.csv carries canopy threshold ", unique(ext$threshold),
           " but loss_by_driver.csv carries ", unique(df$threshold),
           "; the share would divide loss at one threshold by extent at another.")
    }

    absent <- setdiff(unique(df$iso), ext$iso)
    if (length(absent) > 0) {
      stop("GFWLossByDriver: ", length(absent), " countries carry loss but have no extent, e.g. ",
           toString(head(absent, 5)), ".")
    }
    # loss pixels are a subset of the 2000 extent at the same threshold, so cumulative loss cannot
    # exceed it; this is what a version-mismatched pair of files trips
    cum <- stats::aggregate(list(loss = df$loss_ha), by = list(iso = df$iso), FUN = sum)
    both <- merge(cum, ext[, c("iso", "extent_ha")], by = "iso")
    over <- both$loss > both$extent_ha * (1 + 1e-6)
    if (any(over)) {
      stop("GFWLossByDriver: cumulative loss exceeds the 2000 extent for ", sum(over),
           " countries, e.g. ", toString(head(both$iso[over], 5)),
           ". The two files are not from the same version or threshold.")
    }

    ext$extent <- ext$extent_ha / 1e6 # hectares to Mha
    ext$year <- 2000L
    return(magpiesort(as.magpie(ext[, c("iso", "year", "extent")], spatial = 1, temporal = 2)))
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
