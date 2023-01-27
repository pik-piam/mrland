#' @title correctDinerstein2020
#' @description correct data for the Global Safety Net conservation priority
#' areas (Dinerstein et al. 2020).
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readDinerstein2020}}
#' @examples
#' \dontrun{
#' readSource("Dinerstein2020", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctDinerstein2020 <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
