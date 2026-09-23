#' Read ForestLossDrivers
#'
#' Read-in an Forest loss data (range 2001-2015 but only single annual number her)
#' (Source:DOI: 10.1126/science.aau3445 Table 1).
#'
#'
#' @return magpie object of the Curtis et al., 2018 Data
#' @author Abhijeet Mishra
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("ForestLossDrivers")
#' }
#'
#' @importFrom magclass as.magpie
#' @importFrom madrat toolSubtypeSelect
#' @import readxl
#' @import countrycode
#' @importFrom stats complete.cases

readForestLossDrivers <- function() {
  mapping <- read.csv("mapping.csv", header = TRUE, sep = ",")
  isoCountry <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "madrat")
  fullMapping <- merge(mapping, isoCountry, by = "CountryCode")[, c(-2, -4)]
  colnames(fullMapping) <- c("CountryCode", "RegionCodeSource", "RegionCodeMAgPIE")

  # Table 1 of Curtis et al. (2018): tree cover loss 2001-2015 in Mha, driver shares in per cent.
  # Rows are normalised to sum to 100: the printed rows sum to 99-102 (rounding, and cells printed
  # as "<1%"), while the drivers below must sum to the observed loss rather than exceed it. So
  # Russia/China/South Asia shifting agriculture is 0 here, not the "<1%" the paper prints.
  file <- "forest_loss.csv"
  df <- read.csv(file = file, header = TRUE, sep = ",")
  # per cent to share (/ 100), 2001-2015 total to annual (/ 15)
  df[, -c(1:3)] <- (df$treecoverloss_01_15 * (df[, -c(1:3)] / 100)) / 15
  dfMag <- as.magpie(df[, -c(2, 3)], temporal = NULL, spatial = "region")

  # disaggregate to countries, weighted by FRA 2020 naturally regenerating forest area (Mha).
  # Same year calcForestLossShare divides by (2010), so every country in a source region carries
  # one rate; a different year here rescales each country by its own area ratio instead.
  a <- readSource("FRA2020", "forest_area", convert = TRUE)[, , "naturallyRegeneratingForest"]
  a <- collapseNames(a)
  forestLoss <- toolAggregate(x = dfMag, weight = setYears(a[, "y2010", ], NULL), rel = fullMapping,
                              from = "RegionCodeSource", to = "CountryCode")

  return(forestLoss)
}
