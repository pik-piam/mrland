# not useful for gridded urban

convertUrbanLandGao <- function(x) {
  # aggregate to iso-level
  x <- dimSums(x, dim = c("x", "y"))

  return(x)
}
