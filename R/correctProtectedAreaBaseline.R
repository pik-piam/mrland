#' @title correctProtectedAreaBaseline
#' @description correct protected area baseline data
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readProtectedAreaBaseline}}
#' @examples
#' \dontrun{
#' readSource("ProtectedAreaBaseline", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctProtectedAreaBaseline <- function(x) {

  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
