#' @title calcForestLossShare
#'
#' @description Calculates which share of forest area is lost per year to each driver of
#' forest loss.
#'
#' @param source Driver data to use, \code{"GFW"} (default) or \code{"Curtis"}, see
#' \code{\link{calcForestLossByDriver}}
#' @param period Years averaged into the annual rate, GFW only
#'
#' @details The denominator is tree cover extent in 2000 at the same canopy threshold for
#' \code{source = "GFW"} and FRA 2020 naturally regenerating forest area in 2010 for
#' \code{source = "Curtis"}. Shares above 1 are clamped to 1; for a driver column this also
#' warns with the country names.
#'
#' @return MAgPIE object with the share of forest area lost per year by driver
#' @author Abhijeet Mishra, Michael Crawford
#' @importFrom magclass setYears setNames getItems
#' @importFrom utils head
#' @importFrom madrat calcOutput readSource
#' @seealso \code{\link{calcForestLossByDriver}}
#' @examples
#' \dontrun{
#' calcOutput("ForestLossShare", aggregate = FALSE)
#' calcOutput("ForestLossShare", source = "Curtis", aggregate = FALSE)
#' }

calcForestLossShare <- function(source = "GFW", period = 2015:2024) {

  lostArea <- calcOutput("ForestLossByDriver", source = source,  # nolint: undesirable_function_linter.
                         period = period, aggregate = FALSE)

  forestArea <- switch(
    source,  # nolint: undesirable_function_linter.
    "GFW" = readSource("GFWLossByDriver", subtype = "extent", convert = TRUE),
    "Curtis" = readSource("FRA2020", subtype = "forest_area",
                          convert = TRUE)[, "y2010", "naturallyRegeneratingForest"],
    stop("calcForestLossShare: unknown source '", source,  # nolint: undesirable_function_linter.
         "'. Use \"GFW\" or \"Curtis\".")
  )
  forestArea <- setYears(setNames(forestArea, NULL), NULL)

  lostShare <- lostArea / forestArea
  lostShare[is.infinite(lostShare)] <- 0
  lostShare[is.na(lostShare)] <- 0
  # overall (FRA fire area) exceeds 1 in some savanna countries and is not read by MAgPIE; a driver
  # share above 1 means numerator and denominator disagree
  drivers <- setdiff(getItems(lostShare, 3), "overall")
  over <- lostShare[, , drivers] > 1
  if (any(over)) {
    warning("calcForestLossShare: ", sum(over), " driver share(s) above 1 clamped to 1 (",
            source, "): ",  # nolint: undesirable_function_linter.
            toString(utils::head(getItems(lostShare, 1)[apply(over, 1, any)], 5)), ".")
  }
  lostShare[lostShare > 1] <- 1

  return(list(x = lostShare,
              weight = forestArea,
              unit = "1",
              description = paste0("Share of forest area lost per year by driver (",
                                   source, ")"),  # nolint: undesirable_function_linter.
              isocountries = FALSE))
}
