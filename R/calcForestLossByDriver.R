#' @title calcForestLossByDriver
#'
#' @description Forest area lost per year by driver, in Mha, from the Global Forest Watch driver
#' product of Sims et al. (2025) (\code{source = "GFW"}, default) or from Table 1 of Curtis et al.
#' (2018) (\code{source = "Curtis"}), together with the FRA 2020 forest fire area as
#' \code{overall}.
#'
#' @param source \code{"GFW"} (default) or \code{"Curtis"}
#' @param period Years averaged into the annual rate, GFW only; Curtis carries a single 2001-2015
#' mean
#' @return MAgPIE object with forest area lost per year by driver, Mha
#' @author Abhijeet Mishra, Michael Crawford
#' @importFrom magclass mbind setNames dimSums getYears getItems
#' @importFrom madrat readSource
#' @seealso \code{\link{readGFWLossByDriver}}, \code{\link{readForestLossDrivers}}
#' @examples
#' \dontrun{
#' calcOutput("ForestLossByDriver", aggregate = FALSE)
#' calcOutput("ForestLossByDriver", source = "Curtis", aggregate = FALSE)
#' }

calcForestLossByDriver <- function(source = "GFW", period = 2015:2024) {

  # Only shifting cultivation is passed on to MAgPIE: it leaves the land as forest and MAgPIE does
  # not model it itself. Clearing for agriculture, commodities and settlements removes the forest,
  # logging is MAgPIE's own harvest, wildfire and other natural disturbances are natural processes.
  gfwModelDrivers <- c("shifting_cultivation")

  fao <- setNames(readSource("FRA2020", subtype = "forest_fire", convert = TRUE), "overall")
  fao <- dimSums(fao, dim = 2) / length(getYears(fao)) # mean annual area lost to fire, Mha

  drivers <- switch(
    source,  # nolint: undesirable_function_linter.
    "Curtis" = readSource("ForestLossDrivers"),
    "GFW" = {
      x <- readSource("GFWLossByDriver", convert = TRUE)
      wanted <- paste0("y", period)
      absent <- setdiff(wanted, getYears(x))
      if (length(absent) > 0) {
        stop("calcForestLossByDriver: the GFW record does not cover ", toString(absent),
             ". It runs ", min(getYears(x, TRUE)), "-", max(getYears(x, TRUE)), ".")
      }
      absent <- setdiff(gfwModelDrivers, getItems(x, 3))
      if (length(absent) > 0) {
        stop("calcForestLossByDriver: the GFW source does not carry driver class(es) ",
             toString(absent), ".")
      }
      dimSums(x[, wanted, gfwModelDrivers], dim = 2) / length(wanted)
    },
    stop("calcForestLossByDriver: unknown source '", source,  # nolint: undesirable_function_linter.
         "'. Use \"GFW\" or \"Curtis\".")
  )

  out <- mbind(fao, drivers)

  return(list(x = out,
              weight = NULL,
              unit = "Mha",
              description = paste0("Forest area lost per year by driver (",
                                   source, ")"),  # nolint: undesirable_function_linter.
              isocountries = FALSE))
}
