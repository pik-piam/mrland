#' @title calcIrrecoverableCarbonLand
#'
#' @description Returns land area (Mha) that contains 50 %, 75% and
#' 100% of irrecoverable carbon as defined in Noon et al (2022).
#'
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return List with a magpie object
#' @author Patrick v. Jeetze
#' @seealso
#' \code{\link{readNoon2022}}
#'
#' @examples
#' \dontrun{
#' calcOutput("calcIrrecoverableCarbonLand", aggregate = FALSE)
#' }
#'
#' @importFrom mrcommons toolCoord2Isocell
#'
calcIrrecoverableCarbonLand <- function(cells = "magpiecell") {
  ic <- readSource("Noon2022", convert = "onlycorrect")

  luh2v2 <- calcOutput("LUH2v2",
    landuse_types = "LUH2v2", aggregate = FALSE,
    cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
    selectyears = "y1995"
  )

  # calculate total land area
  landArea <- dimSums(luh2v2, dim = 3)
  getCells(landArea) <- getCells(ic)

  # urban land
  urbanLand <- calcOutput("UrbanLandFuture",
    subtype = "LUH2v2", aggregate = FALSE,
    timestep = "5year", cells = "lpjcell"
  )

  # make sure that irrecoverable carbon land is not greater than total land area minus urban area
  landNoUrban <- setYears(landArea, "y2020") - setCells(urbanLand[, "y2020", "SSP2"], getCells(landArea))
  getYears(landNoUrban) <- getYears(ic)
  # compute mismatch factor
  landMismatch <- setNames(landNoUrban, NULL) / ic
  landMismatch <- toolConditionalReplace(landMismatch, c(">1", "is.na()"), 1)
  # correct irrecoverable carbon data
  out <- ic * landMismatch

  if (cells == "magpiecell") {
    out <- toolCoord2Isocell(out)
  } else if (cells != "lpjcell") {
    stop("Please specify cells argument")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = "Land area that contains 50 %, 75% and 100% of irrecoverable carbon as defined in Noon et al (2022).",
    isocountries = FALSE
  ))
}
