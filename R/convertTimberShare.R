#' Converts timber share
#' Update dd-Jmm-jjjj - Please add comment if changes made here (Abhi)
#' 
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing country disaggregated data
#' @author Abhijeet Mishra
#' @examples
#' 
#' \dontrun{ a <- readSource("TimberShare",convert=FALSE)
#' }
#' @importFrom madrat getConfig
convertTimberShare<-function(x) {
  map <- read.csv("jurgensen_mapping.csv",sep=";")
  map$Region <- as.character(map$Region)
  map$ISO3 <- as.character(map$ISO3)
  map$ISO2 <- as.character(map$ISO2)
  map <- map[!duplicated(map$ISO3), ]
  map <- map[map$Region!="Antarctica",]
  map <- map[!is.na(map$LETTERCODE),]
  y <- toolAggregate(x = x,rel = map,from = "Region",to = "ISO3",dim = 1,weight = NULL,partrel = F)

  y <- toolCountryFill(y, fill = 0)

  return(y)
}
