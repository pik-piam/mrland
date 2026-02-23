#' @title calcTradeBilateralScenarioAdj
#'
#' @description Creates dummy trade scenario adjustments file filld with 0's,
#' actual changes currently implemented in the model on the import supply ratio
#' (imports/domestic supply)
#' @return dummy file with 0's to befilled in MAgPIE
#' @author David M Chen
#'
calcTradeBilateralScenarioAdj <- function() {

  tm <- calcOutput("TradeBilateralFAOHarmonized", aggregate = FALSE, yearly = TRUE)
  tm[] <- 0
  tm <- toolHoldConstantBeyondEnd(tm)

  return(list(x = tm,
              weight = NULL,
              unit = "percentage",
              description = "changes to countries import supply ratio"))
}
