#' calcExoTcDummy
#' 
#' Dummy file for regional exogenous tau path
#' 
#' @return Dummy file for regional exogenous tau path
#' @author Florian Humpenoeder
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}},
#' \code{\link{calcOutput}}
#' @importFrom magclass new.magpie
#' @importFrom utils read.csv2

calcExoTcDummy <- function() {
  iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "mrland"),row.names=NULL)
  iso_country1<-as.vector(iso_country[,"x"])
  names(iso_country1)<-iso_country[,"X"]
  x <- new.magpie(cells_and_regions = iso_country1,years = seq(1995,2150,by=5), names = NULL, fill = 0)
  
  return(list(x=x,
              weight=NULL,
              unit="-",
              description="Dummy file for regional exogenous tau path",
              note='All values in the file are set to 0 if a new regional setup is used'))
}
