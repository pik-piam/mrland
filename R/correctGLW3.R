#' @title correctGLW3
#' @description Replaces NA values and any negative artefacts with zero in
#'   GLW 3 gridded livestock rasters (reference year 2010).
#' @return Magpie object with NA and negative values replaced by 0.
#' @param x magpie object provided by the read function
#' @author Marcos Alves, Bin Lin
#' @seealso \code{\link{readGLW3}}
#' @examples
#' \dontrun{
#'   readSource("GLW3", subtype = "Da_Ct_2010", convert = "onlycorrect")
#' }
#' @importFrom madrat toolConditionalReplace

correctGLW3<- function(x){
  
  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  
  return(x)
}
