#' @title calcGTAPTrade
#' @description calculate trade data from GTAP dataset
#' @param subtype   GTAP subtype
#' @param bilateral  whether bilateral trade data should be calculated
#'
#' @return Trade related data as an MAgPIE object
#' @author Xiaoxi Wang
#' @examples
#' \dontrun{
#' x <- calcGTAP("GTAP7_VXMD")
#' }
#' @importFrom magclass ndim

calcGTAPTrade <- function(subtype = NULL, bilateral = FALSE) {
  out <- readSource(type = "GTAP", subtype = subtype)
  if (!bilateral && (ndim(out, dim = 1) > 1)) {
     out <- dimSums(out, dim = 1.2)
  }
  
  weight <- NULL
  if(grepl("GTAP7", subtype)){
  unit <- "Mio.US$04"
  } else if (grepl("GTAP8", subtype)){
   unit <- "Mio.US$07"
  }
  description <- subtype
  return(list(x = out,
              weight = weight,
              unit = unit,
              description = description))
}
