#' @title correctBrennan2022
#' @description correct data for Critical Connectivity
#' Areas (Brennan et al. 2022).
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readBrennan2022}}
#' @examples
#' \dontrun{
#' readSource("Brennan2022", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctBrennan2022 <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
