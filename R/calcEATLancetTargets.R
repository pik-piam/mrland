#' @title calcEATLancetTargets
#' @description 
#' Calculates minimum and maximum targets for healthy food intake according to reference 
#' recommendations proposed by the EAT-Lancet Commission on healthy diets from sustainable  
#' food systems, specified for different MAgPIE commodities.
#'
#' @param  attributes Attributes of food commodities (available: kcal/d and g/d)
#' 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' 
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readEATLancet}},
#' \code{\link{calcEATLancetDiets}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EATLancetTargets")
#' }
#' @export

calcEATLancetTargets <- function(attributes="kcal/d"){
  
  EAT_rtargets <- readSource(type="EATLancet",subtype="recommend")
  EAT_rtargets[which(is.na(EAT_rtargets))] <- 0
  
  
  #### Mapping to MAgPIE 

  # mapping of EAT Lancet food categories to MAgPIE food commodities
  EAT_food <-    c("legumes","soybeans","fish","poultry","eggs","milk","sugar","oil_palm","oil_veg")
  EAT_tfood15 <- c("t_puls_pro","t_soybean","t_fish","t_livst_chick","t_livst_egg","t_livst_milk","t_sugar","t_oils","t_oils") 
  
  rel_matrix_food <- cbind(EAT_food,EAT_tfood15)
  
  EAT_rtargets_food <- toolAggregate(EAT_rtargets,rel = rel_matrix_food,
                  dim = 3.1,from = "EAT_food",to = "EAT_tfood15", partrel=TRUE)
  
  # mapping of EAT Lancet food groups to MAgPIE diet target groups  
  EAT_group <-    c("nuts_seeds","vegetables","fruits","roots","fg_redmeat")
  EAT_tgroup15 <-    c("t_nutseeds","t_fruitveg","t_fruitveg","t_roots","t_redmeat")
  
  rel_matrix_group <- cbind(EAT_group,EAT_tgroup15)
  
  EAT_rtargets_group <- toolAggregate(EAT_rtargets,rel = rel_matrix_group,
                  dim = 3.1,from = "EAT_group",to = "EAT_tgroup15", partrel=TRUE)
  
  EAT <- mbind(EAT_rtargets_food,EAT_rtargets_group)
  
  
  #### Selection of relevant attributes  
  EAT.out <- collapseNames(EAT[,,attributes])
  
  
  #### Define weights and units
  weight.pop <- collapseNames(calcOutput("Population",aggregate = FALSE)[,"y2010","pop_SSP2"])
  unit <- "kcal or grams wm per capita per day"
  
  return(list(x=EAT.out,
              weight=weight.pop,
              unit=unit,
              description="Minimum and maximum targets for healthy food intake according to the EAT-Lancet Commission")
  )
}

