#' Converts Forestry Production Ratio
#' Update dd-Jmm-jjjj - Please add comment if changes made here (Abhi)
#' 
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing country disaggregated data
#' @author Abhijeet Mishra
#' @examples
#' 
#' \dontrun{ a <- readSource("ForestryProductionRatio",convert=FALSE)
#' }
#' 
convertForestryProductionRatio<-function(x) {
  map <- toolGetMapping(type = "regional", name = getConfig("regionmapping"))
  y <- toolAggregate(x = x,rel = map,from = "RegionCode",to = "CountryCode",dim = 1)
  
  #y <- toolCountryFill(x, fill = NA, BHR="QAT", HKG="CHN", MUS="MDG", PSE="ISR", SGP="MYS", TLS="IDN")
  
  return(y)
}