#' @title calcSeedShare
#' @description Calculates Seed share (seed demand per production)
#'
#' @return List of magpie object with results and weight on country or cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("SeedShare")
#' }
#' @importFrom magpiesets findset

calcSeedShare <- function() {

  massbalance <- calcOutput("FAOmassbalance", aggregate = FALSE)
  kcr         <- findset("kcr")
  seedshr     <-
    dimSums(massbalance[, , "seed"][, , "dm"][, , kcr], dim = c(3.2, 3.3)) /
    dimSums(massbalance[, , "production"][, , "dm"][, , kcr], dim = c(3.2, 3.3))
  weight      <- dimSums(massbalance[, , "production"][, , "dm"][, , kcr], dim = c(3.2, 3.3))

  # assume a seed share of 1 percent for begr and betr
  seedshr[, , c("begr", "betr")] <- 0.01
  weight[, , c("begr", "betr")]  <- 1
  seedshr[is.nan(seedshr)]       <- 0
  weight[seedshr == Inf]         <- 0
  seedshr[seedshr == Inf]        <- 0

  seedshr <- mstools::toolHoldConstantBeyondEnd(seedshr)
  weight  <- mstools::toolHoldConstantBeyondEnd(weight) + 10^-10

  return(list(x           = seedshr,
              weight      = weight,
              unit        = "DM share, weight: production quantity",
              description = "Share of production used as seed"))
}
