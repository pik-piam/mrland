#' Read ForestLossDrivers
#' 
#' Read-in an Forest loss data (range 2001-2015 but only single annual number her) (Source:DOI: 10.1126/science.aau3445 Table 1). 
#' 
#' 
#' @return magpie object of the Curtis et al., 2018 Data 
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("ForestLossDrivers")
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom madrat toolSubtypeSelect
#' @import readxl
#' @import countrycode 
#' @importFrom stats complete.cases

readForestLossDrivers <- function(){

  ## Mapping file
  mapping <- read.csv("mapping.csv",header = TRUE,sep = ";")
  
  ## Magpie standard
  iso_country <- toolGetMapping(type = "regional", name = "h12.csv")
  
  ## Merge Mappings
  full_mapping <- merge(mapping,iso_country,by="CountryCode")[,c(-2,-4)] 
  colnames(full_mapping) <- c("CountryCode","RegionCodeSource","RegionCodeMAgPIE")
    
  ## Original Data
  file = "forest_loss.csv"
  df <- read.csv(file = file,header = TRUE,sep = ",") ## This is Tree Cover loss in Mha at this stage, all drivers are in percentage
  df[,-c(1:3)] <- (df$treecoverloss_01_15*(df[,-c(1:3)]/100))/15 ## Division by 100 because we change from percentage to proportion. Division by 15 to get annual data
  
  df_mag <- as.magpie(df[,-c(2,3)],temporal=NULL,spatial="region") ## This is Tree Cover loss in Mha
  
  ## FRA Forest Area
  a <- collapseNames(readSource("FRA2020","forest_area",convert = TRUE)[,,"naturallyRegeneratingForest"]) ## Convert is set to TRUE For Mha
  
  ## Create iso level data based on forest data as weight
  forest_loss <- toolAggregate(x = df_mag, weight = setYears(a[,"y2015",],NULL),rel = full_mapping,from = "RegionCodeSource",to = "CountryCode")
  
  return(forest_loss)
} 