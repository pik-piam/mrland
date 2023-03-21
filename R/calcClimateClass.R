#' @title calcClimateClass
#' @description fraction of a cell belonging to a given climate classification based on Koeppen Geiger Classification.
#' http://koeppen-geiger.vu-wien.ac.at/.
#' @param source select source from: Koeppen, IPCC, IPCC_reduced
#'
#' @return Clustered MAgPIE object on requested resolution
#' @author Abhijeet Mishra
#'
#' @examples
#' \dontrun{
#' calcOutput("ClimateClass", aggregate = FALSE)
#' }
#'
#' @export

calcClimateClass <- function(source = "Koeppen") { #n olint

  datasource <- source # nolint

  if (datasource == "Koeppen") { # nolint

    x      <- readSource("Koeppen", subtype = "cellular", convert = "onlycorrect")

  } else if (grepl("IPCC", datasource)) { # nolint

    x <- readSource("IPCCClimate", convert = "onlycorrect")
    getNames(x) <- gsub(" ", "_", tolower(getNames(x)))

    if (datasource == "IPCC_reduced") { # nolint
      reduceIPCC  <- toolGetMapping("IPCC2IPCCreduced.csv", type = "sectoral")
      x           <- toolAggregate(x, reduceIPCC, from = "ipcc", to = "ipcc_reduced", dim = 3, partrel = TRUE)
    }

  } else {
    stop("Source inc calcClimateClass unkown.")
  }

  weight <- calcOutput("LandArea", aggregate = FALSE)

  return(list(
    x = x,
    weight = weight,
    unit = "share",
    description = "share of koeppen geiger area",
    isocountries = FALSE))
}
