# not useful for gridded urban


convertUrbanLandGao<-function(x){
  CountryToCell   <- toolGetMapping("CountryToCellMapping.csv", type = "cell", where = "mappingfolder")
  x   <- toolAggregate(x, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)

  return(x)

}
