#' @title calcForestFireLoss
#'
#' @description Calculate how much loss of forest area happens due to fire disturbances based on FRA 2020 data
#'
#' @return MAgPIE object with FRA 2020 forest fire area loss
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{readFRA2020}}
#' 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ForestFireLoss",aggregate=FALSE)
#' }
#' 
#' 

calcForestFireLoss <- function(){
  
  out<-setNames(readSource("FRA2020",subtype = "forest_fire",convert = TRUE),NULL) # Convert=T returns Mha area lost to fire
  
  return(list(x=out,
              weight=NULL,
              unit="Mha",
              description="Area affected by fires in forests",
              isocountries=FALSE))
}