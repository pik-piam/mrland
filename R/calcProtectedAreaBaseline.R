#' @title calcProtectedAreaBaseline
#'
#' @description Returns protected land area (Mha) in terms of cropland, pasture, forest and other
#' land between 1995 and 2020.
#'
#' @param magpie_input Whether data should be transformed (based on LUH2v2 data) to match land use types used in MAgPIE.
#' @param nclasses If \code{magpie_input = TRUE}. Options are either "seven" or "nine". Note that by default,
#' the protected area is reported for urban land and forestry is zero.
#' \itemize{
#' \item "seven" separates primary and secondary forest and includes
#' "crop", "past", "forestry", "primforest", "secdforest", "urban" and "other"
#' \item "nine" adds the separation of pasture and rangelands, as well as a
#' differentiation of primary and secondary non-forest vegetation and therefore returns
#' "crop", "past", "range", "forestry", "primforest", "secdforest", "urban", "primother" and "secdother"
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
calcProtectedAreaBaseline <- function(magpie_input = TRUE, nclasses = "seven", # nolint
                                      cells = "lpjcell") {
  PABaseline <- readSource("ProtectedAreaBaseline", convert = "onlycorrect") # nolint

  if (magpie_input == TRUE) {
    luh2v2 <- calcOutput("LUH2v2",
      landuse_types = "LUH2v2", aggregate = FALSE,
      cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
      selectyears = c("y1995", "y2000", "y2005", "y2010", "y2015")
    )

    # extend the data set to all time steps provided in the protected area data
    # i.e. use the data from the year 2015 for the year 2020.
    luh2v2 <- setCells(mbind(luh2v2,
                             setYears(luh2v2[, "y2015", ],
                                      "y2020")),
                       getCells(PABaseline))

    # calculate total land area
    landArea <- dimSums(luh2v2[, "y1995", ], dim = 3)

    # urban land
    urbanLand <- calcOutput("UrbanLandFuture", subtype = "LUH2v2", aggregate = FALSE,
                            timestep = "5year", cells = "lpjcell")

    # make sure that protected area is not greater than total land area minus urban area
    totPABase   <- dimSums(PABaseline, dim = 3)
    landNoUrban <- landArea - setCells(urbanLand[, getYears(PABaseline), "SSP2"], getCells(PABaseline))
    getYears(landNoUrban) <- getYears(PABaseline)
    # compute mismatch factor
    landMismatch <- setNames(landNoUrban, NULL) / setNames(totPABase, NULL)
    landMismatch <- toolConditionalReplace(landMismatch, c(">1", "is.na()"), 1)
    # correct WDPA data
    PABaseline <- PABaseline * landMismatch # nolint

    if (nclasses %in% c("seven", "nine")) {
    # differentiate primary and secondary forest based on luh2v2 data
    totforestluh  <- dimSums(luh2v2[, , c("primf", "secdf")], dim = 3)
    primforestShr <- luh2v2[, , "primf"] / setNames(totforestluh + 1e-10, NULL)
    secdforestShr <- luh2v2[, , "secdf"] / setNames(totforestluh + 1e-10, NULL)
    # where luh2 does not report forest, but we find forest land in
    # protected area data, set share of secondary forest land to 1
    secdforestShr[secdforestShr == 0 & primforestShr == 0] <- 1
    # multiply shares of primary and secondary non-forest veg with
    # land pools in protected area data set
    primforest <- primforestShr * setNames(PABaseline[, , "forest"], NULL)
    secdforest <- secdforestShr * setNames(PABaseline[, , "forest"], NULL)

    out <- mbind(PABaseline[, , c("crop", "past")],
                 new.magpie(cells_and_regions = getCells(PABaseline),
                            years = getYears(PABaseline),
                            names = "forestry",
                            fill = 0),
                setNames(primforest, "primforest"),
                setNames(secdforest, "secdforest"),
                new.magpie(cells_and_regions = getCells(PABaseline),
                           years = getYears(PABaseline),
                           names = "urban",
                           fill = 0),
      PABaseline[, , "other"])

    } else {
      stop("Option specified for argument 'nclasses' does not exist.")
    }

    if (nclasses == "nine") {
      # separate pasture into pasture and rangeland
      totgrassluh <- dimSums(luh2v2[, , c("pastr", "range")], dim = 3)
      pastShr  <- luh2v2[, , "pastr"] / setNames(totgrassluh + 1e-10, NULL)
      rangeShr <- luh2v2[, , "range"] / setNames(totgrassluh + 1e-10, NULL)
      # where luh2 does not report grassland, but we find grassland in
      # protected area data, set share of rangeland to 1
      rangeShr[pastShr == 0 & rangeShr == 0] <- 1
      # multiply shares of pasture and rangeland with pasture in protected area data
      past  <- pastShr * setNames(PABaseline[, , "past"], NULL)
      range <- rangeShr * setNames(PABaseline[, , "past"], NULL)

      # separate other land into primary and secondary
      tototherluh  <- dimSums(luh2v2[, , c("primn", "secdn")], dim = 3)
      primotherShr <- luh2v2[, , "primn"] / setNames(tototherluh + 1e-10, NULL)
      secdotherShr <- luh2v2[, , "secdn"] / setNames(tototherluh + 1e-10, NULL)
      # where luh2 does not report other land, but we find other land in
      # protected area data, set share of secondary other land to 1
      secdotherShr[secdotherShr == 0 & primotherShr == 0] <- 1
      # multiply shares of primary and secondary non-forest veg with other land
      primother <- primotherShr * setNames(PABaseline[, , "other"], NULL)
      secdother <- secdotherShr * setNames(PABaseline[, , "other"], NULL)

      out <- mbind(out[, , "crop"],
                  setNames(past, "past"),
                  setNames(range, "range"),
                  out[, , c("forestry", "primforest", "secdforest", "urban")],
                  setNames(primother, "primother"),
                  setNames(secdother, "secdother"))
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
