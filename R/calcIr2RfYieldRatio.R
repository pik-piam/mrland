#' @title calcIr2RfYieldRatio
#'
#' @description Passes on the irrigated to rainfed yield ratio from AQUASTAT
#' @seealso
#' [readAQUASTAT()],
#' [convertAQUASTAT()]
#' @examples
#'   \dontrun{
#'     calcOutput("Ir2RfYieldRatio")
#'   }
#'
#' @return MAgPIE object of yields
#' @author Kristine Karstens

calcIr2RfYieldRatio <- function() {

  x      <- collapseDim(
              readSource("AQUASTAT", subtype = "rf2irRatio", convert = TRUE),
              dim = c(2, 3))
  weight <- calcOutput("Production", products = "kcr", attributes = "dm",
                       aggregate = FALSE)[, "y1995", ]
  weight <- dimSums(weight, dim = 3)


  return(
    list(x       = x,
    weight       = weight,
    min          = 0,
    unit         = "t per ha per t per ha",
    description  = "Irrigated to rainfed yield ratio based on AQUASTAT"))
}
