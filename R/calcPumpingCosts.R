#' @title calcPumpingCosts
#' @description provides costs of pumping irrigation water
#' @return A magpie object at iso level for all years with information on pumping costs
#' @author Vartika Singh
#' #' @seealso \code{\link{readSource}}, \code{\link{calcOutput}}
#' @importFrom magclass new.magpie
#' @importFrom utils read.csv2
#' @examples
#'
#' \dontrun{
#' calcOutput("PumpingCosts")
#' }
#'

  calcPumpingCosts <- function() {

    isoCountry  <- toolGetMapping("iso_country.csv")
    isoCountry1 <- as.vector(isoCountry[, "x"])
    names(isoCountry1) <- isoCountry[, "X"]
    x <- new.magpie(cells_and_regions = isoCountry1, years = seq(1995, 2100, by = 5), names = "pumpingcost", fill = 0.02)

    weight <- x
    weight[,,] <- 1

  return(list(x            = x,
              weight       = weight,
              unit         = "USD per million cubic meters",
              description  = "costs of pumping irrigation water from Cornish et.al., 2004"))

  }
