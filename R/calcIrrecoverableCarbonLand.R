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

  landIni <- calcOutput("LanduseInitialisation",
    cellular = TRUE, nclasses = "seven", cells = "lpjcell",
    selectyears = "y1995", input_magpie = FALSE, aggregate = FALSE
  )

  # calculate total land area
  landArea <- dimSums(landIni, dim = 3)
  getCells(landArea) <- getCells(ic)

  # urban land
  urbanLand <- calcOutput("UrbanLandFuture",
    subtype = "LUH2v2", aggregate = FALSE,
    timestep = "5year", cells = "lpjcell"
  )

  # make sure that irrecoverable carbon land is not greater than total land area minus urban area
  totNoUrban <- setYears(landArea, "y2020") - setCells(urbanLand[, "y2020", "SSP2"], getCells(landArea))
  getYears(totNoUrban) <- getYears(ic)
  # compute mismatch factor
  landMismatch <- setNames(totNoUrban, NULL) / ic
  landMismatch <- toolConditionalReplace(landMismatch, c(">1", "is.na()"), 1)
  # correct irrecoverable carbon data
  ic <- ic * landMismatch

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
