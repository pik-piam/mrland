#' @title calcNINFruitvegRatio
#' @description
#' Calculates the share of fruits and vegetables in the calorie supply from the others MAgPIE commodity for the past.
#' Information on the calorie supply from fruits and vegetables is relevant in the context of dietary recommendations,
#' e.g. as proposed by the NIN.
#'
#' @param populationweight datasource of populationweight: FAO can be selected in order to better meet exact values.
#' Normal datasource is PopulationPast
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{calcNINTargets}}, \code{\link{calcFAOharmonized}},
#' \code{\link{calcNINDiets}}
#' @examples
#' \dontrun{
#' calcOutput("NINFruitvegRatio")
#' }
#' @importFrom madrat toolFillWithRegionAvg
#' @export

calcNINFruitvegRatio <- function(populationweight="PopulationPast") {

  ### FAO Commodity balance
  FAO_CBS <- calcOutput(type = "FAOharmonized", source = "join2010", aggregate = FALSE)[,,"food_supply_kcal"]
  mag_past <- findset("past_til2020")
  FAO_CBS <- collapseNames(FAO_CBS[, mag_past,])
  getSets(FAO_CBS) <- c("region", "year", "ItemCodeItem")


  ### Population weight
  if (populationweight=="PopulationPast"){
    weight=collapseNames(calcOutput("PopulationPast", aggregate = FALSE))
  }else if (populationweight=="FAO"){
    weight <- collapseNames(readSource(type="FAO", subtype = "Pop", convert = T)[,,"population"])/1000000
  }
  weight<-weight[,mag_past,]


  ### Sectoral mapping for FAO items
  relationmatrix <- toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")
  relationmatrix <- relationmatrix[, c("post2010_FoodBalanceItem", "k")]
  relationmatrix <- relationmatrix[!duplicated(relationmatrix[, "post2010_FoodBalanceItem"]), ]

  ### Definitions of the food groups "others" and "fruitveg_others" (all fruits and vegetables contained in "others")
  others <- relationmatrix[relationmatrix$k %in% "others", "post2010_FoodBalanceItem"]
  fruitveg_others <- c("2601|Tomatoes and products",
                  "2602|Onions",
                  "2605|Vegetables, other",
                  "2611|Oranges, Mandarines",
                  "2612|Lemons, Limes and products",
                  "2613|Grapefruit and products",
                  "2614|Citrus, Other",
                  "2617|Apples and products",
                  "2618|Pineapples and products",
                  "2619|Dates",
                  "2620|Grapes and products (excl wine)",
                  "2625|Fruits, other"
  )
  #Note: In FAOitems.csv 2615|Bananas and 2616|Plantains are mapped to the magpie commodity "cassav_sp".

  #check if all fruits and vegetables included in "fruitveg_others" are also contained in "others":
  if (any(setdiff(fruitveg_others,others)) == TRUE) {
    stop("The above definition of fruits and vegetables (fruitveg_others) is not consistent with the current mapping of FAO items to MAgPIE commodities.")
  }

  ### Aggregation of calories and calculation of ratios:
  others_kcal <- dimSums(FAO_CBS[,,others],dim = 3)
  fruitveg_kcal <- dimSums(FAO_CBS[,,fruitveg_others],dim = 3)

  fruitveg2others_kcal_ratio <- fruitveg_kcal/others_kcal
  out <- toolFillWithRegionAvg(fruitveg2others_kcal_ratio, weight = weight, verbose = FALSE)

  min <- 0
  max <- 1


  return(list(x=out,
              weight=weight,
              unit="-",
              description="share of fruits and vegetables in the others food group",
              min=min
              )
  )
}
