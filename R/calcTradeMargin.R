#' @title calcTradeMargin
#' @description calculate total value of trade margins from GTAP dataset
#' @param gtap_version type of GTAP data version
#' \itemize{
#' \item \code{GTAP7}
#' \item \code{GTAP8}
#' \item \code{GTAP9}
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
#' @importFrom GDPuc convertGDP
#' @importFrom mstools toolCountryFillBilateral

calcTradeMargin <- function(gtap_version = "GTAP9", bilateral = FALSE, producer_price = "FAOini") { # nolint
  stopifnot(gtap_version %in% c("GTAP7", "GTAP8", "GTAP9"))
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
  y <- (vtwr / vxmd * (vom / voa))
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
    p <- collapseNames(calcOutput("PriceAgriculture",
                                  datasource = producer_price, aggregate = FALSE))[, 2005, ]
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

  # take 2x the median by product and (exporting) region
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "madrat")
  reg <- unique(mapping$RegionCode)
  getSets(out)[1] <- "Regions"
  for (i in getNames(out)) {
    for (r in reg) {
      tmp <- out[list("Regions" = mapping[which(mapping$RegionCode == r), "CountryCode"]), , i]
      # round to include values that were in range of e-14
      tmp[which(round(tmp, 3) == 0)]  <- 2 * median(tmp)
      out[list("Regions" =  mapping[which(mapping$RegionCode == r), "CountryCode"]), , i] <- tmp
    }
  }
  # take successively higher percetile by product only for if still 0

  for (qu in c(0.75, 0.85, 0.95)) {

    for (i in magclass::where(out == 0)$true$data) {
      tmp <- out[, , i]
      tmp[which(tmp == 0)]  <- quantile(tmp, qu)
      out[, , i] <- tmp
    }
  }

  weight <- (vxmd * voa)[, , kTrade]

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
  # (distiller's grain an exception as it is in GTAP but not traded in FAOSTAT)
  out <- add_columns(x = out, addnm =  setdiff(findset("k_trade"), getNames(out)), dim = 3.1)
  weight <- add_columns(x = weight, addnm =  setdiff(findset("k_trade"), getNames(weight)), dim = 3.1)
  out[, , "brans"] <- out[, , "maiz"]
  weight[, , "brans"] <- weight[, , "maiz"]
  out[, , "ethanol"] <- out[, , "oils"]
  weight[, , "ethanol"] <- weight[, , "oils"]
  out[, , "scp"] <- out[, , "sugar"]
  weight[, , "scp"] <- weight[, , "sugar"]
  out[, , "distillers_grain"] <- out[, , "tece"]
  weight[, , "distillers_grain"] <- weight[, , "tece"]

  if (gtap_version == "GTAP9") {
      out <- GDPuc::convertGDP(out, unit_in = "current US$MER",
                               unit_out = "constant 2017 US$MER",
                               replace_NAs = "no_conversion")
    
    out <- setYears(out[, 2011, ], NULL)
    weight <- setYears(weight[, 2011, ], NULL)
  } else {
    out <- setYears(out, NULL)
    weight <- setYears(weight, NULL)
  }

  unit <- "US$2017"
  description <- "Trade margins"

  # add tiny value to weight to avoid 0 weights creating 0 values
  weight <- weight + 1e-8

  return(list(x = out,
              weight = weight,
              unit = unit,
              description = description))
}
