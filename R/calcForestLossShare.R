#' @title calcForestLossShare
#'
#' @description Calculates which share of forest area is lost per year to each driver of
#' forest loss.
#'
#' @param src Driver data to use, \code{"GFW"} (default) or \code{"Curtis"}, see
#' \code{\link{calcForestLossByDriver}}
#' @param period Years averaged into the annual rate, GFW only
#'
#' @details The denominator is FRA 2020 naturally regenerating forest area, the natural forest
#' MAgPIE applies the share to: 2020 for \code{src = "GFW"}, 2010 for \code{src = "Curtis"}
#' (loss 2001-2015). Shares above 1 are clamped to 1.
#'
#' @return MAgPIE object with the share of forest area lost per year by driver
#' @author Abhijeet Mishra, Michael Crawford
#' @seealso \code{\link{calcForestLossByDriver}}
#' @examples
#' \dontrun{
#' calcOutput("ForestLossShare", aggregate = FALSE)
#' calcOutput("ForestLossShare", src = "Curtis", aggregate = FALSE)
#' }

calcForestLossShare <- function(src = "GFW", period = 2015:2024) {

  lostArea <- calcOutput("ForestLossByDriver", src = src, period = period, aggregate = FALSE)

  # FRA year in the loss period: Curtis covers 2001-2015, GFW 2015-2024 by default
  year <- if (src == "Curtis") "y2010" else "y2020"
  forestArea <- readSource("FRA2020", subtype = "forest_area",
                           convert = TRUE)[, year, "naturallyRegeneratingForest"]
  forestArea <- setYears(setNames(forestArea, NULL), NULL)

  lostShare <- lostArea / forestArea
  lostShare[is.infinite(lostShare)] <- 0
  lostShare[is.na(lostShare)] <- 0
  lostShare[lostShare > 1] <- 1

  return(list(x = lostShare,
              weight = forestArea,
              unit = "1",
              description = paste0("Share of forest area lost per year by driver (", src, ")"),
              isocountries = FALSE))
}
