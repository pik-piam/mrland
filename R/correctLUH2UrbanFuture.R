#' @title correctLUH2UrbanFuture
#' @description correct LUH2v2 urban future data
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readLUH2UrbanFuture}}
#' @examples
#' \dontrun{
#' readSource("LUH2UrbanFuture", convert = "onlycorrect")
#' }
#' @importFrom madrat toolConditionalReplace
correctLUH2UrbanFuture<-function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()", "< 0"), replaceby = 0)

return(x)
}
