#' @title readREMIND
#' @description Reads in a reporting mif file from REMIND
#' 
#' @param subtype Either "intensive" or "extensive"
#' @param rev data revision the output will be produced for (positive numeric).
#' @return MAgPIE object with regional aggregation of REMIND H12
#' @author David Klein
#' @seealso
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("REMIND",aggregate=FALSE)
#' }
#' @importFrom magclass read.report

readREMIND <- function(subtype, rev=0.1) {
  
  # /p/projects/remind/runs/r8473-trunk-C/output/r8473-trunk-C_*/report.mif
  
  file_list <- c("REMIND_generic_r8473-trunk-C_Budg600-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_Budg950-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_Budg1300-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_NDC-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_NPi-rem-5.mif")
  
  if (rev > 5) {
    file_list <- c(file_list,
                   "REMIND_generic_C_SDP-NPi-rem-5.mif",
                   "REMIND_generic_C_SDP-PkBudg900-rem-5.mif",
                   "REMIND_generic_C_SDP-PkBudg1000-rem-5.mif",
                   "REMIND_generic_C_SDP-PkBudg1100-rem-5.mif",
                   "REMIND_generic_C_SSP1-NPi-rem-5.mif",     
                   "REMIND_generic_C_SSP1-PkBudg900-rem-5.mif",
                   "REMIND_generic_C_SSP1-PkBudg1100-rem-5.mif",
                   "REMIND_generic_C_SSP1-PkBudg1300-rem-5.mif",
                   "REMIND_generic_C_SSP2-NPi-rem-5.mif",
                   "REMIND_generic_C_SSP2-PkBudg900-rem-5.mif",
                   "REMIND_generic_C_SSP2-PkBudg1100-rem-5.mif",
                   "REMIND_generic_C_SSP2-PkBudg1300-rem-5.mif",
                   "REMIND_generic_C_SSP5-NPi-rem-5.mif",
                   "REMIND_generic_C_SSP5-PkBudg900-rem-5.mif",
                   "REMIND_generic_C_SSP5-PkBudg1100-rem-5.mif",
                   "REMIND_generic_C_SSP5-PkBudg1300-rem-5.mif")
  }
  
  x <- NULL
  for(f in file_list) {
    x <- mbind(x,read.report(f,as.list = FALSE))
  }
  
  # remove model and variable name
  x <- collapseNames(x)
  
  # shorten names of the REMIND scenarios
  getNames(x) <- gsub("r8473-trunk-C_", "R2M41-SSP2-",getNames(x))
  getNames(x) <- gsub("C_(SDP|SSP)", "R21M42-\\1",getNames(x))
  getNames(x) <- gsub("-rem-5","",getNames(x))

  return(x)

}