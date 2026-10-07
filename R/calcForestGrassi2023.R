#' @title calcForestGrassi2023
#' @description Intact and non-intact forest area at 0.5 degree after Grassi et al. (2023). Intact forest
#' follows Potapov et al. (2017) for 2013, with the masks for Canada, Brazil and Russia updated from national
#' information. Grassi et al. use non-intact forest as a proxy of the managed forest that national greenhouse
#' gas inventories report.
#'
#' @return List with a magpie object of intact and non-intact forest area (Mha) on cellular level
#' @author Florian Humpenöder
#' @seealso \code{\link{readForestGrassi2023}}
#' @examples
#' \dontrun{
#' calcOutput("ForestGrassi2023", aggregate = FALSE)
#' }

calcForestGrassi2023 <- function() {

  x <- readSource("ForestGrassi2023", convert = "onlycorrect")

  return(list(x = x,
              weight = NULL,
              unit = "Mha",
              description = "Intact and non-intact forest area (Grassi et al. 2023)",
              isocountries = FALSE))
}
