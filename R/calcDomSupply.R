#' Calculate regional domestic supply for trade module
#'
#' Simple function to get the domestic supply for the trade module
#' @title calcTradeMargin
#' @return magpie object domestic supply
#' @param nutrient "dm"
#' @author David Chen
#' @examples
#' \dontrun{
#' a <- calcDomSupply()
#' }
#'
calcDomSupply <- function(nutrient = "dm") {

  out <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , nutrient][, , "domestic_supply"])

  return(list(x = out,
              weight = NULL,
              unit = "Mt",
              description = "Domestic supply"))
}
