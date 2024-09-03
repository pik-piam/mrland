#' @title calcLossShare
#' @description Calculates share of domestic supply wasted
#'
#' @return List of magpie object with results and weight on country or cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("LossShare")
#' }
#' @importFrom magpiesets findset

calcLossShare <- function() {

  massbalance <- calcOutput("FAOmassbalance", aggregate = FALSE)

  # connected to domestic supply, as FAOSTATS flossary states that
  # "Waste is often estimated as a fixed percentage of availability, the latter
  # being defined as production plus imports plus stock withdrawals."
  wasteshr <-
    dimSums(massbalance[, , "waste"][, , "dm"], dim = c(3.2, 3.3)) /
    dimSums(massbalance[, , "domestic_supply"][, , "dm"], dim = c(3.2, 3.3))
  weight <-
    dimSums(massbalance[, , "domestic_supply"][, , "dm"], dim = c(3.2, 3.3))
  newproducts <- c("begr", "betr", "scp")

  # assume a waste share of 1 percent for begr, betr and scp
  wasteshr[, , newproducts] <- 0.01
  weight[, , newproducts]   <- 1
  wasteshr[, , "pasture"]   <- 0
  wasteshr[is.na(wasteshr)] <- 0
  weight[wasteshr == Inf]   <- 0
  wasteshr[wasteshr == Inf] <- 0

  wasteshr <- toolHoldConstantBeyondEnd(wasteshr)
  weight   <- toolHoldConstantBeyondEnd(weight) + 10^-10

  return(list(x           = wasteshr,
              weight      = weight,
              unit        = "DM share, weight: domestic supply",
              description = "Share of domestic supply wasted"))
}
