#' @title correctNoon2022
#' @description correct irrecoverable carbon data from Noon et al. (2022).
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readNoon2022}}
#' @examples
#' \dontrun{
#' readSource("Noon2022", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctNoon2022 <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
