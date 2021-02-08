#' @title calcPlantedForest
#' @description 
#' Calculates the share of plantations in planted forest
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("PlantedForest")
#' }
#' @importFrom magclass getNames<- as.magpie time_interpolate
#' @export

calcPlantedForest <- function(){
  
  ## Read land area frpom source
  a <- readSource("FRA2020","forest_area")
  planted_share <- setNames(round(a[,,"plantationForest"],3)/round(a[,,"plantedForest"],3),NULL)
  planted_share[is.na(planted_share)] <- 0
  out <- setYears(planted_share[,"y2000",],NULL)
  
  ## Weight
  weight <- setYears(setNames(round(a[,"y2000","plantedForest"],3),NULL),NULL)
  
  return(list(x = out,
              weight = weight,
              min = 0,
              unit = "share",
              description = "Calculates the share of plantation forest in planted forest"))
  
}
