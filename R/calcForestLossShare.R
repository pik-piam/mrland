#' @title calcForestLossShare
#'
#' @description Calculates which share of forest land is lost due to different drivers
#'
#' @return MAgPIE object with share of area lost in forests due to different drivers
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{readForestLossDrivers}}
#' 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ForestLossShare",aggregate=FALSE)
#' }
#' 
#' 

calcForestLossShare <- function(){
  
  lost_area   <- calcOutput("ForestFireLoss",aggregate=FALSE) # Convert=T returns Mha area lost to fire
  forest_area <- setYears(setNames(readSource("FRA2020",subtype = "forest_area",convert = TRUE)[,"y2010","naturallyRegeneratingForest"],NULL),NULL) # Convert=T returns Mha area of forests

  lost_share <- lost_area/forest_area
  lost_share[is.infinite(lost_share)] <- 0
  lost_share[is.na(lost_share)] <- 0
  lost_share[lost_share>1] <- 1
  
  out <- lost_share
  weight <- forest_area
  
  return(list(x=out,
              weight=weight,
              unit="1",
              description="Area lost in forests by drivers",
              isocountries=FALSE))
}
