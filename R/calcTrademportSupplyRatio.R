#' @title calcTradeImportSupplyRatio
#'
#' @description Calculates regional imports to supply ratios
#' often termed "Import Dependency Ratio"
#' @return Self import to dupply ratio
#' @author David M Chen
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}}
#' @examples
#' \dontrun{
#' a <- calcTradeImportSupplyRatio()
#' }
#'
calcTradeImportSupplyRatio <- function() {

  mb <- calcOutput("FAOmassbalance_pre", aggregate = FALSE)
  tm <- calcOutput("FAOBilateralTrade", output = "qty",
                   products = "kcr", fiveYear = FALSE,
                   aggregate = FALSE)
  tm1 <- calcOutput("FAOBilateralTrade", output = "qty",
                    products = "kli", fiveYear = FALSE,
                    aggregate = FALSE)
  tm2 <- calcOutput("FAOBilateralTrade", output = "qty",
                    products = "kothers", fiveYear = FALSE,
                    aggregate = FALSE)
  tmfo <- calcOutput("TimberTradeBilateral", aggregate = FALSE)
  tm <- mbind(tm, tm1)
  tm <- mbind(tm, tm2)
  rm(tm1, tm2)

  ## hold the timber bilat constant into past and future, as they just missing a few years
  tmfo <- toolHoldConstant(tmfo,
                           years = c(tail(getYears(tmfo, as.integer = TRUE), n = 1):
                                       tail(getYears(tm, as.integer = TRUE), n = 1)))
  tmfo <- toolHoldConstant(tmfo, years = c(1995:1996))
  tmfo[, 1995, ] <- tmfo[, 1996, ] <- tmfo[, 1997, ]
  cyears <- intersect(getYears(tm), getYears(tmfo))

  tm <- mbind(tm, tmfo)

  tmi <- dimSums(tm, dim = 1.2)

  mbx <- mb[, , "export"][, , "dm", drop = TRUE]
  mbi <- mb[, , "import"][, , "dm", drop = TRUE]

  cyears <- intersect(getYears(tm), getYears(mb))
  citems <- intersect(getItems(tm, dim = 3), getItems(mb, dim = 3.1))
  missing <- setdiff(findset("k_trade"), citems)
  missing
  # sodistillers grain, ethanol, scp, fish are now still  missing completely from the bilateral trade matrix
  # the first 3 are made up producsts so don't exist in FAO either. Need to include fish at some point


  # here we look at difference between trade matrix and mass balance and scale the trade matrix to mb

  imDiff <- tmi[, cyears, citems] / mbi[, cyears, citems]

  imDiff[is.na(imDiff)] <- 1


  imDiff[is.infinite(imDiff)] <- NA

  # scale up imports

  # rename exporters so that we scale up  the Wimporters and not exporters
  getItems(tm, dim = 1.2) <- paste0(getItems(tm, dim = 1.2), "2")

  tmS  <- tm[, cyears, citems] / imDiff
  tmS[is.infinite(tmS)] <- 0
  tmS[is.na(tmS)] <- 0
  # for cases where there is no tmi but trade reported in mbi we still want to match mbi
  # so as assumption split the mbi imports across the bilateral patterns first by taking average of adjacent years
  # then by trade patterns of all products

  # fill based on function wwhich first tries with average between both adjacent years,
  # then tries one or the other, and then both again
  #  only for where mbi >0 and sum(tm, dim=1.2) i.e. total imports  =0

  .fillMiss <- function(tm, year = "both") {
    # amounts missing
    totMiss <- ifelse(dimSums(tm, dim = 1.2) == 0 & mbi[, cyears, citems] > 0,
                      mbi[, cyears, citems],
                      0)
    yearAdjTrPatterns <- new.magpie(cells_and_regions = getItems(tm, dim = 1),
                                    years = getYears(tm),
                                    names = getNames(tm),
                                    fill = 0)

    for (t in getYears(tm, as.integer = TRUE)[-c(1, length(getYears(tm)))]) {

      if (year == "both") {
        yearAdjTrPatterns[, t, ] <-      (setYears(tm[, t - 1, ] / dimSums(tm[, t - 1, ], dim = 1.2), NULL) +
                                            setYears(tm[, t + 1, ] / dimSums(tm[, t + 1, ], dim = 1.2), NULL)) / 2
      } else if (year == "after") {
        yearAdjTrPatterns[, t, ] <-      setYears(tmS2[, t + 1, ] / dimSums(tmS2[, t + 1, ], dim = 1.2), NULL)
      } else if (year == "before") {
        yearAdjTrPatterns[, t, ] <-      setYears(tmS2[, t - 1, ] / dimSums(tmS2[, t - 1, ], dim = 1.2), NULL)
      }
    }

    toFill <- totMiss * yearAdjTrPatterns
    toFill[is.na(toFill)] <- 0

    out <- tm + toFill
    return(out)
  }

  # make out object
  tmSout <- tmS

  for (i in seq(1:5)) {
    # state round
    print(paste("round", i))
    # first both
    tmS2 <- .fillMiss(tmSout, year = "both")
    # then after
    tmS3 <- .fillMiss(tmS2, year = "after")
    # then before
    tmSout <- .fillMiss(tmS3, year = "before")

    # check
    z <- dimSums(tmSout, dim = 1.2) - mbi[, cyears, citems]
    print(paste(nrow(magclass::where(round(z, 8) < 0)$true$individual), "still not matching mb"))
  }

  missing <- magclass::where(round(z, 3) < 0)$true$individual
  missing <- as.vector(as.data.frame(missing))

  # give exporters the 2 suffix
  getItems(mbx, dim = 1) <- paste0(getItems(mbx, dim = 1), "2")
  tmSout1 <- tmSout
  tmSout <- tmSout1
  # fill with global expoerters ratio
  for (i in seq_along(length(missing[[1]]))) {
    tmSout[missing$im[i], missing$Year[i], missing$ItemCodeItem[i]] <-
      mbi[missing$im[i], missing$Year[i], missing$ItemCodeItem[i]] *
      mbx[, missing$Year[i], missing$ItemCodeItem[i]] /
      dimSums(mbx[, missing$Year[i], missing$ItemCodeItem[i]], dim = 1)
  }



  ratio <- round(tmSout[, cyears, citems], 6) / 
    round(mb[, cyears, citems][, , "dm"][, , "domestic_supply"], 6)
  ratio[is.na(ratio)] <- 0
  ratio[is.infinite(ratio)] <- 0
  getItems(ratio, dim = 1.2) <- gsub("[0-9]+", "", getItems(ratio, dim = 1.2))

  ratio <- collapseNames(ratio)

  ratio2 <- time_interpolate(ratio1, interpolated_year = seq(1965, 1990, 5),
                             integrate_interpolated_years = TRUE,
                             extrapolation_type = "constant")
  ratio2 <- time_interpolate(ratio2, interpolated_year = seq(2025, 2150, 5),
                             integrate_interpolated_years = TRUE,
                             extrapolation_type = "constant")

  t <- magpiesets::findset("t_all")
  ratio2 <- ratio2[, t, ]
  ratio2 <- dimOrder(ratio2z, dim = 1, perm = c(2, 1))
  citems <- intersect(getItems(ratio2, dim = 3), getItems(mb, dim = 3.1))
  weight <- mb[, , citems][, , "dm", drop = TRUE][, , "domestic_supply", drop = TRUE]
  weight <- toolHoldConstantBeyondEnd(weight)
  cyears <- intersect(getYears(ratio2), getYears(weight))
  weight <- weight[, cyears, ]  


  return(list(x = ratio2,
              weight = weight,
              unit = "ratio",
              description = "countries' import supply ratio. Imports/Domestic supply"))
}
