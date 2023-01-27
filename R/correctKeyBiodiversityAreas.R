#' @title correctKeyBiodiversityAreas
#' @description correct data for Key Biodiversity Areas.
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readKeyBiodiversityAreas}}
#' @examples
#' \dontrun{
#' readSource("KeyBiodiversityAreas", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctKeyBiodiversityAreas <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
