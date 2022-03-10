#' @title correctLanduseToolbox
#' @description correct Landuse Toolbox output data
#' @return corrected magpie object
#' @param x magpie object provided by the read function
#' @author David Hoetten
#' @seealso
#'   \code{\link{readLanduseToolbox}}
#' @examples
#' \dontrun{
#' A <- readSource("LanduseToolbox", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctLanduseToolbox <- function(x) {
  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  return(x)
}
