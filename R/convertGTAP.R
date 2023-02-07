#' @title convertGTAP
#' @description Converts GTAP data to fit to the common country list. Weighting is done by using the Imports and
#' Exports from FAO. NOW NEW WEIGHTING
#'
#' @param x MAgPIE object contains GTAP data
#' @param subtype The GTAP subtype: VIWS, VIMS VXWD, VXMD, VOA, VOM
#' @return Converted GTAP Data
#' @author Xiaoxi Wang
#' @examples
#' \dontrun{
#' x <- ReadSource("GTAP", "GTAP7_VIMS")
#' }
#' @importFrom madrat toolAggregate
#' @importFrom magclass as.magpie magpiesort

convertGTAP <- function(x, subtype) {
  x <- magpiesort(x)

  gtap <- paste0(gsub("(?<=\\d)\\w{1,}", "", subtype, perl = TRUE))
  subtype <- unlist(strsplit(subtype, "_"))[[2]]

  fao <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, 2005, "dm"])

  if (gtap == "GTAP7") {
    regMapping <- toolGetMapping(type = "regional", name = "GTAP7Mapping2016.csv", where = "mrland")
  } else if (gtap == "GTAP8") {
    regMapping <- toolGetMapping(type = "regional", name = "GTAP8Mapping2016.csv", where = "mrland")
  }

  sectorMapping <- toolGetMapping(type = "sectoral", name = "mappingGTAPMAgPIETrade.csv", where = "mrland")
  sectorMapping <- sectorMapping[which(sectorMapping$gtap != "xxx" & sectorMapping$magpie != "zzz"), ]

  if (subtype %in% c("VIWS", "VIMS", "VXWD", "VXMD")) {
    faoImport <- collapseNames(fao[, 2005, "import"])
    faoExport <- collapseNames(fao[, 2005, "export"])
    if (subtype %in% c("VXWD", "VXMD")) {
      w1 <- toolAggregate(faoExport, rel = regMapping, from = "Country.code", to = "Region.code",
                          dim = 1, partrel = TRUE)
      w1 <- toolAggregate(w1, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)

      w2 <- toolAggregate(faoImport, rel = regMapping, from = "Country.code", to = "Region.code",
                          dim = 1, partrel = TRUE)
      w2 <- toolAggregate(w2, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)

      w3 <- toolAggregate(faoExport, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)
      # make w3 bilateral for later
      w4 <- w3
      w4[] <- 1
      getItems(w4, 1) <- paste0(getItems(w4, 1), "a")
      w3 <- w3 * w4
      getItems(w3, dim = 1.2) <- substr(getItems(w3, dim = 1.2), 1, 3)

      out <- toolAggregate(x[, , getNames(w1)], rel = regMapping, from = "Region.code", to = "Country.code",
                           dim = 1.1, weight = w1)
      out <- toolAggregate(out[, , getNames(w2)], rel = regMapping, from = "Region.code", to = "Country.code",
                           dim = 1.2, weight = w2)
      out <- toolAggregate(out, rel = sectorMapping, from = "gtap", to = "magpie", dim = 3,
                           weight = w3[getItems(out, dim = 1), , ], partrel = TRUE)
    } else if (subtype %in% c("VIWS", "VIMS")) {
      w1 <- toolAggregate(faoImport, rel = regMapping, from = "Country.code", to = "Region.code",
                          dim = 1, partrel = TRUE)
      w1 <- toolAggregate(w1, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)

      w2 <- toolAggregate(faoExport, rel = regMapping, from = "Country.code", to = "Region.code",
                          dim = 1, partrel = TRUE)
      w2 <- toolAggregate(w2, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)

      w3 <- toolAggregate(faoImport, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)
      # make w3 bilateral for later
      w4 <- w3
      w4[] <- 1
      getItems(w4, 1) <- paste0(getItems(w4, 1), "a")
      w3 <- w3 * w4
      getItems(w3, dim = 1.2) <- substr(getItems(w3, dim = 1.2), 1, 3)
      # aggregate first region dim based on import weight
      out <- toolAggregate(x[, , getNames(w1)], rel = regMapping, from = "Region.code", to = "Country.code",
                           dim = 1.1, weight = w1)
      # aggregate second region dim based on export weight
      out <- toolAggregate(out[, , getNames(w2)], rel = regMapping, from = "Region.code", to = "Country.code",
                           dim = 1.2, weight = w2)
      # aggregate sector based on imports?
      out <- toolAggregate(out, rel = sectorMapping, from = "gtap", to = "magpie", dim = 3,
                           weight = w3[getItems(out, dim = 1), , ], partrel = TRUE)
    }
  } else if (subtype %in% c("VOM", "VOA")) {
    faoProduction <- collapseNames(fao[, , "production"])
    w1 <- toolAggregate(faoProduction, rel = regMapping, from = "Country.code", to = "Region.code",
                        dim = 1, partrel = TRUE)
    w1 <- toolAggregate(w1, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)

    w2 <- toolAggregate(faoProduction, rel = sectorMapping, from = "magpie", to = "gtap", dim = 3, partrel = TRUE)

    out <- toolAggregate(x[, , getNames(w1)], rel = regMapping, from = "Region.code", to = "Country.code",
                         dim = 1, weight = w1)
    out <- toolAggregate(out, rel = sectorMapping, from = "gtap", to = "magpie", dim = 3,
                         weight = w2[getItems(out, dim = 1.1), , ], partrel = TRUE)
  } else {
    (stop("Not supported by current convertGTAP funtion, please set convert to FALSE!"))
  }

# fill with global average
if (ndim(out, dim = 1) == 2) {

   out <- toolCountryFillBilateral(out, fill = 0)

} else if (ndim(out, dim = 1) == 1) {
out <- toolCountryFill(out, 0)
}

  return(out)
}