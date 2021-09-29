#' @title calcIrrigationInvCosts
#' 
#' @description This function calculates irrigation investment costs for each country until the year 2050. 
#' Values linearly converge towards the value of Germany (1995) by 2050.
#' 
#' @return MAgPIE object
#' 
#' @author Nele Steinmetz, Felicitas Beier
#' 
#' @seealso \code{\link{calcOutput}}, \code{\link{readWBirrigation}},
#' \code{\link{convertWBirrigation}}
#' 
#' @examples
#' \dontrun{ 
#' calcOutput("IrrigationInvCosts")
#' }
#' 
#' @import magclass 
#' 
calcIrrigationInvCosts <- function() {
  
  # Investment costs for expanding irrigation infrastructure in US$ per hectare
  WBirrigation           <- readSource("WBirrigation")
  getYears(WBirrigation) <- NULL
  
  # irrigation cost constant until 2015
  data              <- new.magpie(getRegions(WBirrigation), 1995:2050, "ad_unit_cost")
  data[,1995:2050,] <- WBirrigation
  # conversion: $1995 to $2004
  data              <- data * 1000 * 1.19
  
  # from 2015 onwards, data converges to value of Germany until 2050
  data_DEU <- new.magpie(getRegions(WBirrigation), 1995:2050, "ad_unit_cost", fill=as.numeric(data["DEU","y1995",]))
  
  years <- new.magpie("GLO", getYears(data), NULL, getYears(data,as.integer=TRUE))                                                 
  pos        <- (years - 2015) / (2050 - 2015)
  pos[pos<0] <- 0
  pos[pos>1] <- 1
  
  data <- data_DEU*pos + data*(1-pos)
  
  # expand this value until 2150
  time_extend    <- paste0("y", seq(2055,2150,5))
  data           <- time_interpolate(data, time_extend, extrapolation_type="constant", integrate_interpolated_years=TRUE)
  getNames(data) <- NULL
  
  # aggregation weight and transform from 1000ha to ha
  w <- readSource("FAO_online", subtype="Land", convert=T)[,1995,"6610|Agricultural land.Area_(1000_ha)"] * 1000
  
  return(list(x=data,
              weight=w,
              unit="US$2004", 
              description="unit cost for irrigation expansion"))
}
