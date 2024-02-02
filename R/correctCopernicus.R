#' @title correctCopernicus
#' @description correct Copernicus data.
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readCopernicus}}
#' @examples
#' \dontrun{
#' readSource("Copernicus", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctCopernicus <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
