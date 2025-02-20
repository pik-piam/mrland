#' @importFrom magclass setNames getNames


calcProcessing_conversion_factors<-function(){
  massbalance<-calcOutput("FAOmassbalance_pre", version = "post2010", aggregate = F)

  kpr<-findset("kpr")
  ksd<-findset("ksd")
  kprocessing<-findset("processing20")
  mb_reduced<-dimSums(massbalance[,,"dm"],dim=c(1,3.3))
  kmb<-paste("X",kpr,sep="")
  
  production_estimated<-dimSums(mb_reduced[,,"production_estimated"][,,ksd],dim=c(3.2))
  convmatrix<-add_dimension(x = production_estimated,dim = 3.2,add = "kpr",nm = kmb)
  convmatrix<-add_dimension(x = convmatrix,dim = 3.1,add = "processing",nm = kprocessing)
  convmatrix[,,]<-0
  
  mb_reduced2<-mb_reduced[,,kpr]
  
  tmp <- dimSums(mb_reduced2[,,c("alcohol1","alcohol2","alcohol3", "alcohol4")],dim = c(3.2))/dimSums(mb_reduced2[,,c("fermentation")],dim = c(3.2))
  convmatrix[,,"alcohol"][,,"fermentation"]<-setNames(tmp,paste0("X",getNames(tmp)))  
  
  tmp <- dimSums(mb_reduced2[,,c("brewers_grain1")],dim = c(3.2))/dimSums(mb_reduced2[,,c("fermentation")],dim = c(3.2))
  convmatrix[,,"distillers_grain"][,,"fermentation"]<-setNames(tmp,paste0("X",getNames(tmp))) 
  
  tmp <- dimSums(mb_reduced2[,,c("brans1")],dim = c(3.2))/dimSums(mb_reduced2[,,c("milling")],dim = c(3.2))
  convmatrix[,,"brans"][,,c("milling")]<-setNames(tmp,paste0("X",getNames(tmp)))
  
  tmp <- dimSums(mb_reduced2[,,c("branoil1")],dim = c(3.2))/dimSums(mb_reduced2[,,c("milling")],dim = c(3.2))
  convmatrix[,,"oils"][,,"milling"]<-setNames(tmp,paste0("X",getNames(tmp))) 
  
  tmp <- dimSums(mb_reduced2[,,c("distillers_grain1")],dim = c(3.2))/dimSums(mb_reduced2[,,"distilling"],dim = c(3.2))
  convmatrix[,,"distillers_grain"][,,"distilling"]<-setNames(tmp,paste0("X",getNames(tmp)))   
  
  tmp <- dimSums(mb_reduced2[,,c("ethanol1")],dim = c(3.2))/dimSums(mb_reduced2[,,c("distilling")],dim = c(3.2))
  convmatrix[,,"ethanol"][,,"distilling"]<-setNames(tmp,paste0("X",getNames(tmp))) 
  
  tmp <- dimSums(mb_reduced2[,,c("molasses1")],dim = c(3.2))/dimSums(mb_reduced2[,,c("refining")],dim = c(3.2))
  convmatrix[,,"molasses"][,,"refining"]<-setNames(tmp,paste0("X",getNames(tmp))) 
  
  tmp <- dimSums(mb_reduced2[,,c("sugar1", "sugar2")],dim = c(3.2))/dimSums(mb_reduced2[,,c("refining")],dim = c(3.2))
  convmatrix[,,"sugar"][,,"refining"]<-setNames(tmp,paste0("X",getNames(tmp)))   
  
  tmp <- dimSums(mb_reduced2[,,c("oilcakes1")],dim = c(3.2))/dimSums(mb_reduced2[,,c("extracting")],dim = c(3.2))
  convmatrix[,,"oilcakes"][,,"extracting"]<-setNames(tmp,paste0("X",getNames(tmp))) 
  
  tmp <- dimSums(mb_reduced2[,,c("oil1","oil2")],dim = c(3.2))/dimSums(mb_reduced2[,,c("extracting")],dim = c(3.2))
  convmatrix[,,"oils"][,,"extracting"]<-setNames(tmp,paste0("X",getNames(tmp))) 
  
  # add conversion attributes of Single cell Protein (SCP)
  # based on Table S3 in Pikaar et al 2018, which provides conversion factors in as "ton substrate DM / ton microbial protein (MP)"
  # Miscanthus (begr): 5.5 t DM begr / t DM MP
  # Sugar Cane (sugr_cane): 4.3 t DM sugar_cane / t DM MP
  # maize (foddr): 5.6 t DM foodr / t DM MP
  # We need to convert these factors into "Conversion factors of primary products into secondary products"
  # How much MP (secondary product) do get per ton DM substrate (primary product)?
  # For each t DM sugr_cane we get 1/4.3=0.2326 t DM MP.
  convmatrix[,,"breeding"][,,"scp"][,,"Xbegr"]<-1/5.5 #0.1818
  convmatrix[,,"breeding"][,,"scp"][,,"Xsugr_cane"]<-1/4.3 #0.2326
  convmatrix[,,"breeding"][,,"scp"][,,"Xfoddr"]<-1/5.6 #0.1786
  
  tmp<-collapseNames(dimSums(massbalance[,,"production"][,,"dm"][,,"fibres"],dim=c(1))/dimSums(massbalance[,,"production"][,,"dm"][,,"cottn_pro"],dim=c(1)))
  convmatrix[,,"ginning"][,,"fibres"][,getYears(tmp),"Xcottn_pro"]=tmp
  convmatrix[is.nan(convmatrix)]<-0
  convmatrix[is.na(convmatrix)]<-0
  
  # add conversion attributes of bioenergy crops into ethanol
  
  convmatrix[,,"extracting"][,,"ethanol"][,,"Xbetr"] <- 0.36
  convmatrix[,,"extracting"][,,"ethanol"][,,"Xbegr"] <- 0.36
  
  #test
  test=T
  if(test==T){
    kprocessingM<-setdiff(kprocessing,c("breeding","ginning"))
    mb_reduced<-dimSums(massbalance[,,"dm"],dim=c(3.3))
  
    a<-mb_reduced[,,kprocessingM][,,kpr]       
    getNames(a,dim=1)<-paste0("X",getNames(a,dim=1))
    b<- dimSums(a*convmatrix[,,kprocessingM],dim=c(3.1,3.2))
    c<- dimSums(mb_reduced[,,ksd][,,"production_estimated"],dim=3.2)
    if (any(round(b-c,4)!=0)) {stop("conversion factors are incoherent with production_estimated column of massbalance!")}
  }
  
  getNames(convmatrix,dim=3)<-substring(getNames(convmatrix,dim=3),2)
  
  convmatrix <- toolHoldConstantBeyondEnd(convmatrix)
  
  return(list(x=convmatrix,weight=NULL,unit="t DM / t DM",description="Conversion factors of primary products into secondary products. primary product x conversion factor = secondary product"))
}
