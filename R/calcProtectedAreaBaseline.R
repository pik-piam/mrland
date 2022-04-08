#' @title calcProtectedAreaBaseline
#'
#' @description Returns protected land area (Mha) in terms of cropland, pasture, forest and other land between 1995 and 2020.
#'
#' @param magpie_input Whether data should be transformed (based on LUH2v2 data) to match land use types used in MAgPIE.
#' @param nclasses If \code{magpie_input = TRUE}. Options are either "seven" or "nine". Note that by default, the protected area is reported for urban land and forestry is zero.
#' \itemize{
#' \item "seven" separates primary and secondary forest and includes "crop", "past", "forestry", "primforest", "secdforest", "urban" and "other"
#' \item "nine" adds the separation of pasture and rangelands, as well as a differentiation of primary and secondary non-forest vegetation and therefore returns "crop", "past", "range", "forestry", "primforest", "secdforest", "urban", "primother" and "secdother"
#' }
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return List with a magpie object
#' @author Patrick v. Jeetze
#' @seealso
#' \code{\link{calcProtectArea}}
#'
#' @examples
#' \dontrun{
#' calcOutput("ProtectedAreaBaseline", aggregate = FALSE)
#' }
#'
#' @importFrom mrcommons toolCoord2Isocell
#'
calcProtectedAreaBaseline <- function(magpie_input = TRUE, nclasses = "seven", cells = "magpiecell") {
  PABaseline <- readSource("ProtectedAreaBaseline", convert = "onlycorrect")

  if (magpie_input == TRUE) {
    LUH2v2 <- calcOutput("LUH2v2",
      landuse_types = "LUH2v2", aggregate = FALSE,
      cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
      selectyears = c("y1995", "y2000", "y2005", "y2010", "y2015")
    )

    # extend the data set to all time steps provided in the protected area data
    # i.e. use the data from the year 2015 for the year 2020.
    LUH2v2 <- mbind(
      LUH2v2,
      setYears(LUH2v2[, "y2015", ], "y2020")
    )

    # differentiate primary and secondary forest based on LUH2v2 data
    totforest_luh <- dimSums(LUH2v2[, , c("primf", "secdf")], dim = 3)
    primforest_shr <- LUH2v2[, , "primf"] / setNames(totforest_luh + 1e-10, NULL)
    secdforest_shr <- LUH2v2[, , "secdf"] / setNames(totforest_luh + 1e-10, NULL)
    # where luh2 does not report forest, but we find forest land in
    # protected area data, set share of secondary forest land to 1
    secdforest_shr[secdforest_shr == 0 & primforest_shr == 0] <- 1
    # multiply shares of primary and secondary non-forest veg with
    # land pools in protected area data set
    primforest <- setCells(primforest_shr, getCells(PABaseline)) * setNames(PABaseline[, , "forest"], NULL)
    secdforest <- setCells(secdforest_shr, getCells(PABaseline)) * setNames(PABaseline[, , "forest"], NULL)

    out <- mbind(
      PABaseline[, , c("crop", "past")],
      new.magpie(getCells(PABaseline), getYears(PABaseline), "forestry", fill = 0),
      setNames(primforest, "primforest"),
      setNames(secdforest, "secdforest"),
      new.magpie(getCells(PABaseline), getYears(PABaseline), "urban", fill = 0),
      PABaseline[, , "other"]
    )

    if (nclasses == "nine") {

      # separate pasture into pasture and rangeland
      totgrass_luh <- dimSums(LUH2v2[, , c("pastr", "range")], dim = 3)
      past_shr <- LUH2v2[, , "pastr"] / setNames(totgrass_luh + 1e-10, NULL)
      range_shr <- LUH2v2[, , "range"] / setNames(totgrass_luh + 1e-10, NULL)
      # where luh2 does not report grassland, but we find grassland in
      # protected area data, set share of secondary other land to 1
      range_shr[past_shr == 0 & range_shr == 0] <- 1
      # multiply shares of primary and secondary non-forest veg with corrected other land
      past <- setCells(past_shr, getCells(PABaseline)) * setNames(PABaseline[, , "past"], NULL)
      range <- setCells(range_shr, getCells(PABaseline)) * setNames(PABaseline[, , "past"], NULL)

      # separate other land into primary and secondary
      totother_luh <- dimSums(LUH2v2[, , c("primn", "secdn")], dim = 3)
      primother_shr <- LUH2v2[, , "primn"] / setNames(totother_luh + 1e-10, NULL)
      secdother_shr <- LUH2v2[, , "secdn"] / setNames(totother_luh + 1e-10, NULL)
      # where luh2 does not report other land, but we find other land in
      # protected area data, set share of secondary other land to 1
      secdother_shr[secdother_shr == 0 & primother_shr == 0] <- 1
      # multiply shares of primary and secondary non-forest veg with corrected other land
      primother <- setCells(primother_shr, getCells(PABaseline)) * setNames(PABaseline[, , "other"], NULL)
      secdother <- setCells(secdother_shr, getCells(PABaseline)) * setNames(PABaseline[, , "other"], NULL)

      out <- mbind(
        out[, , "crop"],
        setNames(primother, "past"),
        setNames(secdother, "range"),
        out[, , c("forestry", "primforest", "secdforest", "urban")],
        setNames(primother, "primother"),
        setNames(secdother, "secdother")
      )
    }
  } else {
    out <- PABaseline
  }

  if (cells == "magpiecell") {
    out <- toolCoord2Isocell(out)
  } else if (cells != "lpjcell") {
    stop("Please specify cells argument")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = "Protected land area in terms of cropland, pasture, forest and other land between 1995 and 2020.",
    isocountries = FALSE
  ))
}
