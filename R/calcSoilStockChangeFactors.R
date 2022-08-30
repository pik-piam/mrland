#' @title calcSoilStockChangeFactors
#' @description calculates and merges information on stock change factors
#'
#' @seealso
#' [readIPCC()]
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilStockChangeFactors")
#' }
#'
#' @return MAgPIE object of yields
#' @author Kristine Karstens

calcSoilStockChangeFactors <- function() {

  # x      <- readSource("IPCC")
  # weight <- calcOutput("Production", products = "kcr", attributes = "dm",
  #                      aggregate = FALSE)[, "y1995", ]
  # weight <- dimSums(weight, dim = 3)


  return(
    list(x            = NULL,
         weight       = NULL,
         min          = 0,
         unit         = "tC per tC",
         description  = "Stock change factors for first 30 cm of the soil profile"))
}
