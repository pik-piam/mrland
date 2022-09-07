#' @title calcTradeTariff
#' @description calculate tarde tariffs from GTAP dataset
#' @param gtap_version type of GTAP data version
#' \itemize{
#' \item \code{GTAP7}
#' \item \code{GTAP8}
#' }
#'
#' @param type_tariff which producer price should be used
#' \itemize{
#' \item \code{type_tariff}
#' }
#'
#'@param bilateral calculates whether tariffs should be bilateral 
#'
#' @return Trade tariffs as an MAgPIE object
#' @author Xiaoxi Wang
#' @examples
#'     \dontrun{
#'     x <- calcTradeTariff("GTAP7")
#'     }
#' @importFrom madrat toolAggregate
#' @importFrom reshape2 acast
#' @importFrom magclass as.data.frame add_columns
#' @importFrom magpiesets findset
#'

calcTradeTariff<- function(gtap_version = "GTAP7", type_tariff = "total", bilateral = FALSE){
  
  stopifnot(gtap_version %in% c("GTAP7","GTAP8"))
  vom  <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VOM", sep ="_"),aggregate = FALSE)
  voa  <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VOA", sep ="_"),aggregate = FALSE)
  vxmd <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VXMD", sep ="_"),aggregate = FALSE, bilateral = bilateral)
  viws <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VIWS", sep ="_"),aggregate = FALSE, bilateral = bilateral)
  x <- vom[,,getNames(viws)]/voa[,,getNames(viws)] #output at dom mkt prices/prod payment(farm gate)
  fillMean <- function(x){
    stopifnot(is.magpie(x))
    for (k in getNames(x)){
      mean <- mean(x[,,k],na.rm = TRUE)
      x[,,k][is.na(x[,,k])] <- mean
    }
    return(x)
  }

  x <- fillMean(x)
  if (type_tariff == "export"){
    #export tax: positive values are taxes, negative values are export subsidies

    vxwd <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VXWD",sep="_"), aggregate = FALSE, bilateral = bilateral)
    y <- (vxwd-vxmd)/vxmd #(exports World Price - export mkt price)/export mkt price 
    y[is.nan(y)] <-NA
  }else if (type_tariff == "import"){
    #import tariff: positive values are tariffs

    vims <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VIMS",sep="_"), aggregate = FALSE, bilateral = bilateral)
    y <- (vims - viws)/vxmd #(imports mkt Price - imports wld price)/export mkt price
    y[is.nan(y)] <- NA
    y[is.infinite(y)] <- NA
  }else if (type_tariff == "total"){
    vxwd <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VXWD",sep="_"), aggregate = FALSE, bilateral = bilateral)
    vims <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VIMS",sep="_"), aggregate = FALSE, bilateral = bilateral)
    y<- (vxwd-vxmd + vims - viws)/vxmd
    y[is.nan(y)] <- NA
    y[is.infinite(y)] <- NA
  }else  {stop(paste("type_tariff",type_tariff, "is not supported!"))}

if(bilateral){
    x <- add_dimension(x, dim = 1.2,   nm = getRegions(x))
}

  y <- fillMean(y)*x #multiply tariff ratio by exporting country's value ratio

  k_trade <- findset("k_trade")
  missing <- setdiff(k_trade,getNames(y))
  y <- add_columns(y,dim = 3.1,addnm = missing)
  y[,,missing]=0
  y<- y[,,setdiff(getNames(y),k_trade),invert=TRUE]

  p <- collapseNames(calcOutput("PriceAgriculture",datasource = "FAO",aggregate = FALSE))[,2005,]

  p_glo<-calcOutput("IniFoodPrice", aggregate=FALSE, products="k_trade")

  missing2 <- setdiff(getNames(p_glo),getNames(p))
  p <- add_columns(p,dim=3.1,addnm =missing2 )

  for (i in missing2){
    p[,,i] <- as.numeric(p_glo[,,i])
    }

if(bilateral){
  p <- add_dimension(p, dim = 1.2,   nm = getRegions(p))
}
  out <- setYears(y[,,k_trade],NULL)*p[,,k_trade] #multiply tariff by exporter producer price
 if(bilateral){
  out <- toolCountryFillBilateral(out,0)
 } else{
  out <- toolCountryFill(out, 0)
 }
  


  # set export trade tariffs of ruminant meant in Inida high to prevent exports; Japan too
    #exporting region is indicated by dim 1.1 with set name "Regions", importers are "REG"
  out[list(Regions = "IND"),,"livst_rum"]   <- 10^3
  out[list(Regions = "JPN"),,"livst_rum"]   <- 10^5
  out[list(Regions = "JPN"),,"livst_pig"]   <- 10^5
  out[list(Regions = "JPN"),,"livst_chick"] <- 10^5
  out[list(Regions = "JPN"),,"livst_egg"]   <- 10^5
  out[list(Regions = "JPN"),,"livst_milk"]  <- 10^5
  # set trade tariffs of alcohol in Japan high to prevent unrealistic exports
  out[list(Regions = "JPN"),,"alcohol"]  <- 10^5

  # use sugar tariffs as a surrogate for sugar crops
  out[,,"sugr_cane"] <- setNames(out[,,"sugar"],"sugr_cane")
  out[,,"sugr_beet"] <- setNames(out[,,"sugar"],"sugr_beet")

if(bilateral){
    voa_w <- add_dimension(voa, dim = 1.2,   nm = getRegions(voa))
    weight <- setYears(toolCountryFillBilateral(vxmd*voa[,,getNames(viws)],0),NULL)
} else{
  weight <- setYears(toolCountryFill(vxmd*voa[,,getNames(viws)],0),NULL)
}
  missing <- setdiff(k_trade,getNames(weight))
  weight <- add_columns(weight,dim=3.1,addnm = missing)
  weight[,,missing] <- 1
  weight <- weight[,,setdiff(getNames(weight),k_trade),invert=TRUE]


  description <- paste0(type_tariff," trade tariff")
  unit <- "USD05MER/tDM"

  return(list(x=out,
              weight = weight,
              unit = unit,
              description = description))

}
