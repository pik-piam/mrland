#' @title correctForestGrassi2023
#' @description Sets cells without data in the forest map of Grassi et al. (2023) to zero.
#' @param x magpie object provided by the read function
#' @return magpie object on cellular level
#' @author Florian Humpenöder
#' @seealso \code{\link{readForestGrassi2023}}
#' @examples
#' \dontrun{
#' readSource("ForestGrassi2023", convert = "onlycorrect")
#' }
#' @importFrom madrat toolConditionalReplace

correctForestGrassi2023 <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)
  return(x)
}
