#' @title calc2ndBioDem
#' @description calculates 2nd generation bioenergy demand
#' @return magpie object with results on country level, weight on country level, unit and description.
#' 
#' @param rev data revision the output will be produced for (positive numeric)
#' 
#' @examples   
#' 
#' \dontrun{ 
#' calcOutput("2ndBioDem")
#' }
#' @import magclass
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass collapseNames time_interpolate mbind

calc2ndBioDem <- function(datasource, rev = 0.1) {
  
  if (datasource == "REMIND") {
    x <- readSource("REMIND", subtype = paste0("extensive_",rev))
    x <- x[,,"Primary Energy Production|Biomass|Energy Crops (EJ/yr)"]*10^3
    x <- collapseNames(x)
    first_remind_year <- sort(getYears(x))[1]
    x <- time_interpolate(x,seq(1995,2150,5),extrapolation_type = "constant")
    
    # set values in initial years that are not existing in REMIND data to zero
    x[,getYears(x)<first_remind_year,]<-0
    
    description <- "2nd generation bioenergy demand for different scenarios taken from R2M41 coupled runs"
    
  } else if (datasource == "REMMAG") {
    x <- readSource("REMMAG","biodem")
    #harmonize historic period
    x[,c(1995,2000,2005,2010),] <- collapseNames(x[,c(1995,2000,2005,2010),"SSP2-Ref-SPA0"])
    description <- "2nd generation bioenergy demand for different scenarios taken from R17M3 coupled runs"

  } else if (datasource == "SSPResults") {
    x<-readSource("SSPResults")
    x<- collapseNames(x[,,"Primary Energy|Biomass|Energy Crops (EJ/yr)"])*10^3
    description <- "2nd generation bioenergy demand for different scenarios taken from IIASA SSP database"
    
  } else if (datasource == "S4N_project") {
    # Total bioenergy demand (including 1st genation, 2nd generation and residues) at country level from IMAGE for 2 different SSP2 scenarios (in EJ per year)
    x <- readSource("S4Nproject_input", subtype="bioenergy", convert="onlycorrect")
    # Transform units: from PJ to EJ
    x <- x*1e3
    
    # 1st gen BE demand in MAgPIE in scenario selected for Sim4Nexus (in PJ/yr)
    BE_1st <- calcOutput("1stBioDem", years=seq(2005, 2100,by=5), aggregate=FALSE)
    BE_1st <- collapseNames(BE_1st[,,"const2030"])
    BE_1st <- dimSums(BE_1st, dim=3)
    # 2nd gen residues in MAgPIE in scenario selected for Sim4Nexus (in PJ/yr)
    res    <- calcOutput("ResFor2ndBioengery", products="kres", product_aggr=TRUE, add_off=TRUE, years=seq(2005, 2100,by=5), aggregate=FALSE)
    res    <- collapseNames(res[,,"ssp2"])
    
    # 2nd generation bioenergy demand: Total BE (IMAGE) - 1st BE (MAgPIE) - residues (MAgPIE)
    x <- x - BE_1st - res
    
    # Correct negative values
    x[x<0] <- 0
    
    #x<- collapseNames(x[,,"Primary Energy|Biomass|Energy Crops (EJ/yr)"])*10^3
    description <- "2nd generation bioenergy demand for different scenarios provided by IMAGE"
    
  } else if (datasource == "SSP_and_REM") {
    ssp <- calcOutput("2ndBioDem",datasource="SSPResults",aggregate = FALSE, rev = rev)
    rem <- calcOutput("2ndBioDem",datasource="REMIND",aggregate = FALSE, rev = rev)
    
    ssp <- time_interpolate(ssp,getYears(rem),extrapolation_type = "constant")
    x <- mbind(ssp,rem)
    
    # sort scenarios alphabetically
    x <- x[,,sort(getNames(x))]
    
    description <- "2nd generation bioenergy demand for different scenarios taken from R2M41 coupled runs and from IIASA SSP database"
  
  } else { 
    stop("Unknown datasource",datasource)
  }
  
  return(list(x=x, weight=NULL, 
              description=description,
              unit="PJ per year", 
              note="bioenergy is demanded in the country which is expected to produce the bioenergy (demand after trade)"))
  
}