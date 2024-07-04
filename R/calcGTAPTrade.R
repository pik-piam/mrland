#' @title calcGTAPTrade
#' @description calculate trade data from GTAP dataset
#' @param subtype   GTAP version and subtype, separated by "_"
#' available versions are "GTAP7", "GTAP8", and "GTAP9"
#' GTAP sheets relevant for trade are "VIWS": Trade - Bilateral Imports at World Prices
#'        "VIMS": Trade - Bilateral Imports at Market Prices
#'        "VXWD": Trade - Bilateral Exports at World Prices
#'        "VXMD": Trade - Bilateral Exports at Market Prices
#'        "VDFM": Intermediates - Firms' Domestic Purchases at Market Prices
#'        "VIFM": Intermediates - Firms' Imports at Market Prices
#'        "VFM": Endowments - Firms' Purchases at Market Prices
#'        "VOA": Payment received by producers (fram gtate value)
#'        "VOM": Value of output at dometic market prices
#' @param bilateral  whether bilateral trade data should be calculated
#' @return Trade related data as an MAgPIE object
#' @author Xiaoxi Wang, David M Chen
#' @examples
#' \dontrun{
#' x <- calcGTAP("GTAP7_VXMD")
#' }
#' @importFrom magclass ndim

calcGTAPTrade <- function(subtype = NULL, bilateral = FALSE) {

  split <- toolSplitSubtype(subtype, list(version = NULL, header = NULL),
                            sep = "_")

  if (split$version == "GTAP9") {
    # version 9 still needs conversion from GTAP products to magpie products, in
    sectorMapping <- toolGetMapping(type = "sectoral", name = "mappingGTAPMAgPIETrade.csv", where = "mrland")
    sectorMapping <- sectorMapping[which(sectorMapping$gtap != "xxx" & sectorMapping$magpie != "zzz"), ]

    fao <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , "dm"])
    fao <- time_interpolate(fao, interpolated_year = c(2004:2011),
                            integrate_interpolated_years = TRUE)
    fao[fao < 0] <- 0

    if (split$header %in% c("VXWD", "VXMD", "VIWS", "VIMS")) {
      out <- readSource("GTAPv8v9", subtype = paste0("9:", split$header))

      if (split$header %in% c("VXWD", "VXMD")) {

        w <- toolAggregate(collapseNames(fao[, getYears(out), "export"]),
                           rel = sectorMapping, from = "magpie", to = "gtap",
                           dim = 3, partrel = TRUE)

      } else if (split$header %in% c("VIWS", "VIMS")) {

        out <- readSource("GTAPv8v9", subtype = paste0("9:", split$header))

        w <- toolAggregate(collapseNames(fao[, getYears(out), "import"]),
                           rel = sectorMapping, from = "magpie", to = "gtap",
                           dim = 3, partrel = TRUE)
      }

      # make w bilateral
      w2 <- w
      w2[] <- 1
      getItems(w2, 1) <- paste0(getItems(w2, 1), "a")
      w <- w * w2
      getItems(w, dim = 1.2) <- substr(getItems(w, dim = 1.2), 1, 3)

    } else if (split$header %in% c("VOM", "VOA")) {
      # VOM and VOA are part of the same dataset in GTAP9
      out <- readSource("GTAPv8v9", subtype = "9:CM04")
      if (split$header == "VOM") {
        out <- dimSums(out, dim = 3.2)
      } else {
        out <- dimSums(out[, , "prodrev"], dim = 3.2)
      }

      w <- toolAggregate(collapseNames(fao[, getYears(out), "production"]),
                         rel = sectorMapping, from = "magpie", to = "gtap",
                         dim = 3, partrel = TRUE)
    }

    out <- toolAggregate(out, rel = sectorMapping,
                         from = "gtap", to = "magpie", dim = 3,
                         weight = w[getItems(out, dim = 1), getYears(out), ] + 10^-10,
                         partrel = TRUE)


    weight <- NULL
    unit <- "million current US$"

  } else {
    out <- readSource(type = "GTAP", subtype = subtype)
    weight <- NULL
    if (grepl("GTAP7", subtype)) {
      unit <- "Mio.US$04"
    } else if (grepl("GTAP8", subtype)) {
      unit <- "Mio.US$07"
    }
  }

  if (!bilateral && (ndim(out, dim = 1) > 1)) {
    out <- dimSums(out, dim = 1.2)
  }

  description <- subtype
  return(list(x = out,
              weight = weight,
              unit = unit,
              description = description))
}
