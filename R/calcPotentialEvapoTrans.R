#' @title calcPotentialEvapoTrans
#' @description Calculates or loads potential Evapotranspiration
#'
#' @param climatetype Switch between different climate scenarios
#' @param datasource Source of the data. Available at the moment: LPJmLClimateInput
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("PotentialEvapoTrans", aggregate = FALSE,
#'            datasource = "LPJmLClimateInput:LPJmL4_for_MAgPIE_44ac93de:harmonized2020")
#' }
#'
#' @importFrom SPEI thornthwaite

calcPotentialEvapoTrans <-
  function(climatetype = "MRI-ESM2-0:ssp370",
           datasource = "LPJmLClimateInput:LPJmL4_for_MAgPIE_44ac93de:harmonized2020") {

  if (grepl("LPJmLClimateInput", datasource)) {

    type <- toolSplitSubtype(datasource, list(calcfunction = "LPJmLClimateInput",
                                           lpjmlVersion = NULL,
                                           stage        = NULL))

    temperature <- calcOutput("LPJmLClimateInput",
                              climatetype  = climatetype,
                              variable     = "temperature:monthlyMean",
                              stage        = type$stage,
                              lpjmlVersion = type$lpjmlVersion,
                              aggregate = FALSE)
    latitude    <- toolGetMappingCoord2Country(pretty = TRUE)$lat

    # estimate potential evapotranspiration using the thornwaite method
    # # for temperature and latitude
    pet         <- toolThornthwaite(temperature, latitude)

  } else if (grepl("LPJmL_new", datasource)) {

    type <- toolSplitSubtype(datasource, list(calcfunction = "LPJmL_new",
                                          lpjmlVersion = NULL,
                                          stage        = NULL))

    pet <- calcOutput("LPJmL_new",
                      version = type$lpjmlVersion,
                      climatetype = climatetype,
                      subtype = "mpet",
                      stage =  type$stage,
                      aggregate = FALSE)

  } else {
 stop("datasource argument unknown. Please check function help.")
 }

  return(list(x            = pet,
              weight       = NULL,
              unit         = "millimeters of water per month",
              description  = paste0("potential evapotranspiration from ",
                                    type$calcfunction),
              isocountries = FALSE))
}
