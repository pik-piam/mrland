#' @title calcForestDisturbances
#'
#' @description Calculates which share of forest land is lost due to forest disturbances
#' (including insects, diseases, severe weather events and other causes)
#'
#' @return MAgPIE object with FRA 2020 forest disturbance shares
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link[mrfaocore]{readFRA2020}}
#'
#' @examples
#' \dontrun{
#' calcOutput("ForestDisturbances", aggregate = FALSE)
#' }

calcForestDisturbances <- function() {
  # convert = TRUE returns Mha area lost to disturbances and Mha forest area
  disturbanceArea <- setNames(readSource("FRA2020", subtype = "disturbance", convert = TRUE), NULL)
  forestArea <- setNames(readSource("FRA2020", subtype = "forest_area",
                                    convert = TRUE)[, , "naturallyRegeneratingForest"], NULL)

  commonYears <- intersect(getYears(disturbanceArea), getYears(forestArea))

  disturbanceShare <- disturbanceArea[, commonYears, ] / forestArea[, commonYears, ]
  disturbanceShare[is.infinite(disturbanceShare)] <- 0
  disturbanceShare[is.na(disturbanceShare)] <- 0
  disturbanceShare[disturbanceShare > 1] <- 1

  out <- setYears(disturbanceShare[, "y2010", ], NULL)
  weight <- setYears(forestArea[, "y2010", ], NULL)

  return(list(x = out,
              weight = weight,
              unit = "1",
              description = paste("Area affected by forest disturbances including insects, diseases,",
                                  "severe weather events and other causes"),
              isocountries = FALSE))
}
