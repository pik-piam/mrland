#' Read WBirrigation
#' 
#' Read-in an WBirrigation data .csv file as magclass object
#' from Jones, William I. 1995. "World Bank and Irrigation." Washington, D.C.: World Bank.
#' 
#' @return magpie object of the WBirrigation data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="WBirrigation")
#' }
#' 
readWBirrigation <- function() {    
      irr <- read.csv("irrigation.csv", sep=";", row.names=1)
      regions <- c("East and South Asia"="ESA","East Asia"="EAS", "South Asia"="SAS", "India"="IND", "Europe"="ERP", "Middle East"="MET", "Africa" ="AFR", "North Africa"="NAF", "Sub-Saharan Africa"="SAF", "Latin America and Caribbean"="LAC")
      irr <- data.frame(ad_unit_cost=irr$ad_unit_cost, reg=as.character(regions),stringsAsFactors=F)
      irr <- as.magpie(irr)
      getYears(irr) <- "y1995"
      return(irr)
}  
