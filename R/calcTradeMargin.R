#' @title calcTradeMargin
#' @description calculate total value of trade margins from GTAP dataset
#' @param gtap_version type of GTAP data version
#' \itemize{
#' \item \code{GTAP7}
#' \item \code{GTAP8}
#' }
#' @param producer_price which producer price should be used
#' @param bilateral  whether bilateral trade margin should be calculated
#'
#' @return Trade margins as an MAgPIE object
#' @author Xiaoxi Wang
#' @examples
#' \dontrun{
#' x <- calcTradeMargin("GTAP7")
#' }
#' @importFrom magpiesets findset
#' @importFrom magclass is.magpie

calcTradeMargin <- function(gtap_version = "GTAP7", bilateral = FALSE, producer_price = "FAOini") { # nolint
  stopifnot(gtap_version %in% c("GTAP7", "GTAP8"))
  viws <- calcOutput(type = "GTAPTrade", subtype = paste(gtap_version, "VIWS", sep = "_"),
                     bilateral = bilateral, aggregate = FALSE)
  vxwd <- calcOutput(type = "GTAPTrade", subtype = paste(gtap_version, "VXWD", sep = "_"),
                     bilateral = bilateral, aggregate = FALSE)
   vtwr <- viws - vxwd # imports values at  W price - export value at W Price

  vtwr[vtwr < 0] <- 0

  vxmd <- calcOutput(type = "GTAPTrade", subtype = paste(gtap_version, "VXMD", sep = "_"),
                     bilateral = bilateral, aggregate = FALSE)
  vom <- calcOutput(type = "GTAPTrade", subtype = paste(gtap_version, "VOM", sep = "_"),
                    bilateral = bilateral, aggregate = FALSE)
  voa <- calcOutput(type = "GTAPTrade", subtype = paste(gtap_version, "VOA", sep = "_"),
                    bilateral = bilateral, aggregate = FALSE)

  # (imports-exports world price)/exports at mkt price * (value of output mkt prices/payment received at fgate)
  y <- setYears(vtwr / vxmd * (vom / voa), NULL)

  y[is.infinite(y)] <- NA

  fillMean <- function(x) {
    stopifnot(is.magpie(x))
    for (k in getNames(x)) {
      mean <- mean(x[, , k], na.rm = TRUE)
      x[, , k][is.na(x[, , k])] <- mean
    }
    return(x)
  }

  y <- fillMean(y)

 # NOTE all producer prices except for FAOini have products missing from FAOini. These will get 0 in MAgPIE
  if (is.null(producer_price)) {
    producer_price <- "FAOp" # nolint
  }
  if (producer_price %in% c("IMPACT3.2.2World_Price", "FAO", "FAOp", "WBGEM")) {
    p <- collapseNames(calcOutput("PriceAgriculture", datasource = producer_price, aggregate = FALSE))[, 2005, ]
  } else if (producer_price == "FAOini") {
   p <- calcOutput("IniFoodPrice", products = "k_trade", aggregate = FALSE)
  } else {
(stop("Valid food price is required"))
}

  kTrade <- findset("k_trade")
  kTrade <- intersect(intersect(getNames(p), getNames(y)), kTrade)

if (bilateral && !("GLO" %in% getItems(p, dim = 1))) {
# make a dummy bilateral price object so magclass doesn't get confused
   p1 <- p
   p1[] <- 1
   getItems(p1, dim = 1) <- paste0(getItems(p1, dim = 1), "b")
   p <- p * p1
   getItems(p, dim = 1.2) <- substr(getItems(p, dim = 1.2), 1, 3)
}
  out <- y[, , kTrade] * p[, , kTrade]

 if (bilateral) {
  out <- toolCountryFillBilateral(out, 0)
 } else {
out <- toolCountryFill(out, 0)
}

##### make countries with 0 margins high
# take max by product and (exporting) region
mapping <- toolGetMapping("regionmappingH12.csv", type = "regional")
reg <- unique(mapping$RegionCode)

for (i in getNames(out)) {
  for (r in reg) {
tmp <- out[list("Regions" = mapping[which(mapping$RegionCode == r), "CountryCode"]), , i]
# round to include values that were in range of e-14
tmp[which(round(tmp, 3) == 0)]  <- max(tmp)
out[list("Regions" =  mapping[which(mapping$RegionCode == r), "CountryCode"]), , i] <- tmp
}
}
# take 99 percetile by product for if still 0
for (i in where(out == 0)$true$data) {
tmp <- out[, , i]
tmp[which(tmp == 0)]  <- quantile(tmp, .75)
out[, , i] <- tmp
}

  weight <- setYears(vxmd * voa, NULL)[, , kTrade]
 if (!any(c("wood", "woodfuel") %in% getNames(weight))) {
  out <- add_columns(x = out, addnm = findset("kforestry"), dim = 3.1)
  out[, , findset("kforestry")] <- out[, , "others"]
 }

  if (bilateral) {
  weight <- toolCountryFillBilateral(weight, 0)
} else {
    weight <- toolCountryFill(weight, 0)
}

  if (!any(c("wood", "woodfuel") %in% getNames(weight))) {
  weight <- add_columns(x = weight, addnm = findset("kforestry"), dim = 3.1)

  weight[, , "wood"] <- weight[, , "tece"] * 0.5
  weight[, , "woodfuel"] <- weight[, , "wood"] * 0.5
  }

  # fill in data in ktrade but not in GTAP
  out <- add_columns(x = out, addnm =  setdiff(findset("k_trade"), getNames(out)), dim = 3.1)
  weight <- add_columns(x = weight, addnm =  setdiff(findset("k_trade"), getNames(weight)), dim = 3.1)
  out[, , "brans"] <- out[, , "maiz"]
  weight[, , "brans"] <- weight[, , "maiz"]
  out[, , "ethanol"] <- out[, , "oils"]
  weight[, , "ethanol"] <- weight[, , "oils"]
  out[, , "scp"] <- out[, , "sugar"]
  weight[, , "scp"] <- weight[, , "sugar"]
  out[, , "betr"] <- out[, , "wood"]
  weight[, , "betr"] <- weight[, , "wood"]
  out[, , "begr"] <- out[, , "sugr_cane"]
  weight[, , "begr"] <- weight[, , "sugr_cane"]

  unit <- "US$05"
  description <- "Trade margins"
  out <- setYears(out, 2005)
  weight <- setYears(weight, 2005)

  return(list(x = out,
              weight = weight,
              unit = unit,
              description = description))
}
