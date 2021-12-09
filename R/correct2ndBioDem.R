#' @title correct2ndBioDem
#' @description apply lowpass filter on 2ndBioDem to filter out high frequency data
#' @return magclass object
#' @param x magclass object provided by the read function
#' @author Florian Humpenoeder
#' @seealso
#'   \code{\link{calc2ndBioDem}}
#' @importFrom magclass lowpass

correct2ndBioDem<- function(x){
  
  lowpass <- 3
  
  #years
  years <- getYears(x,as.integer = T)
  yr_hist <- years[years > 1995 & years <= 2020]
  yr_fut <- years[years >= 2020]
  
  #apply lowpass filter (not applied on 1st time step, applied seperatly on historic and future period)
  x <- mbind(x[,1995,],lowpass(x[,yr_hist,],i=lowpass),lowpass(x[,yr_fut,],i=lowpass)[,-1,])
  
  return(x)
}
