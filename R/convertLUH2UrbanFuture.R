# not useful for gridded urban


convertLUH2UrbanFuture <- function(x){
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  x   <- toolAggregate(x, rel=map, from="coords", to="iso", partrel=TRUE)

  return(x)

}
