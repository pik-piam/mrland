#' @title calcEndUseTimber
#' @description 
#' Calculates the demand of timber from historical FAO data (including intermediate products).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EndUseTimber")
#' }
#' @importFrom magpiesets findset 
#' @export

calcEndUseTimber <- function(){

  fao_timber <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,,"domestic_supply"])
  
  fao_timber <- fao_timber[,findset("past")]
  
  getNames(fao_timber) <- gsub(x = getNames(fao_timber),pattern = " ",replacement = "_")
  getNames(fao_timber) <- gsub(x = getNames(fao_timber),pattern = "-",replacement = "_")
  getNames(fao_timber) <- tolower(getNames(fao_timber))
  
  out <- fao_timber
  
  return(list(x=out,weight=NULL,
                unit="mio m3",
                description="historical timber domestic demand")
         )
}
