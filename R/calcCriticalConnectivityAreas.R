#' @title calcCriticalConnectivityAreas
#'
#' @description Returns unprotected land area (Mha) within Critical Connectivit Areas
#' as given in Brennan et al. (2022).

#' @param maginput Whether data should be transformed (based on LUH2v2 data) to match land use types used in MAgPIE.
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
#' @param mask Whether Key Biodiversity Areas ("KBA") or Global Safety Net and Key Biodiversity Areas
#' ("KBA_GSN") are masked. This switch is useful for complementary scenario building.
#'
#' @return List with a magpie object
#' @author Patrick v. Jeetze
#' @seealso
#' \code{\link{readBrennan2022}}
#'
#' @examples
#' \dontrun{
#' calcOutput("calcCriticalConnectivityAreas", aggregate = FALSE)
#' }
#'
#' @importFrom mstools toolCoord2Isocell
#'
calcCriticalConnectivityAreas <- function(maginput = TRUE, nclasses = "seven",
                                          cells = "lpjcell", mask = "KBA_GSN") {
  if (mask == "KBA") {
    cca <- readSource("Brennan2022", subtype = "KBA_masked", convert = "onlycorrect")
  } else if (mask == "KBA_GSN") {
    cca <- readSource("Brennan2022", subtype = "KBA_GSN_masked", convert = "onlycorrect")
  } else {
    stop("Option specified for argument 'mask' does not exist.")
  }

  if (maginput == TRUE) {
    luh2v2 <- calcOutput("LUH2v2",
      landuse_types = "LUH2v2", aggregate = FALSE,
      cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
      selectyears = "y2015"
    )
    getYears(luh2v2) <- NULL
    getCells(luh2v2) <- getCells(cca)

    # calculate total land area
    landArea <- dimSums(luh2v2, dim = 3)

    # urban land
    urbanLand <- calcOutput("UrbanLandFuture",
      subtype = "LUH2v2", aggregate = FALSE,
      timestep = "5year", cells = "lpjcell"
    )

    # make sure that cca land is not greater than total land area minus urban area
    landNoUrban <- setYears(landArea, "y2020") - setCells(urbanLand[, "y2020", "SSP2"], getCells(landArea))
    getYears(landNoUrban) <- getYears(cca)
    # compute mismatch factor
    ccaTotalLand <- dimSums(cca[, , "CCA"], dim = 3.2)
    landMismatch <- setNames(landNoUrban, NULL) / ccaTotalLand
    landMismatch <- toolConditionalReplace(landMismatch, c(">1", "is.na()"), 1)
    # correct cca data
    cca[, , "CCA"] <- cca[, , "CCA"] * landMismatch[, , "CCA"]

    if (nclasses %in% c("seven", "nine")) {
      # differentiate primary and secondary forest based on LUH2v2 data
      totForestLUH <- dimSums(luh2v2[, , c("primf", "secdf")], dim = 3) # nolint
      primforestShr <- luh2v2[, , "primf"] / setNames(totForestLUH + 1e-10, NULL)
      secdforestShr <- luh2v2[, , "secdf"] / setNames(totForestLUH + 1e-10, NULL)
      # where luh2 does not report forest, but we find forest land in
      # CCA data, set share of secondary forest land to 1
      secdforestShr[secdforestShr == 0 & primforestShr == 0] <- 1
      # multiply shares of primary and secondary non-forest veg with
      # land pools in CCA data set
      primforest <- setNames(primforestShr, NULL) * cca[, , paste(getNames(cca, dim = 1), "forest", sep = ".")]
      secdforest <- setNames(secdforestShr, NULL) * cca[, , paste(getNames(cca, dim = 1), "forest", sep = ".")]

      out <- mbind(
        cca[, , c("crop", "past")],
        new.magpie(getCells(cca), getYears(cca), paste(getNames(cca, dim = 1), "forestry", sep = "."), fill = 0),
        setNames(primforest, paste(getNames(cca, dim = 1), "primforest", sep = ".")),
        setNames(secdforest, paste(getNames(cca, dim = 1), "secdforest", sep = ".")),
        new.magpie(getCells(cca), getYears(cca), paste(getNames(cca, dim = 1), "urban", sep = "."), fill = 0),
        cca[, , "other"]
      )
    } else {
      stop("Option specified for argument 'nclasses' does not exist.")
    }

    if (nclasses == "nine") {
      # separate pasture into pasture and rangeland
      totGrassLUH <- dimSums(luh2v2[, , c("pastr", "range")], dim = 3) # nolint
      pastShr <- luh2v2[, , "pastr"] / setNames(totGrassLUH + 1e-10, NULL)
      rangeShr <- luh2v2[, , "range"] / setNames(totGrassLUH + 1e-10, NULL)
      # where luh2 does not report grassland, but we find grassland in
      # CCA data, set share of rangeland to 1
      rangeShr[pastShr == 0 & rangeShr == 0] <- 1
      # multiply shares of pasture and rangeland with pasture in CCA data
      past <- setNames(pastShr, NULL) * cca[, , paste(getNames(cca, dim = 1), "past", sep = ".")]
      range <- setNames(rangeShr, NULL) * cca[, , paste(getNames(cca, dim = 1), "past", sep = ".")]

      # separate other land into primary and secondary
      totOtherLUH <- dimSums(luh2v2[, , c("primn", "secdn")], dim = 3) # nolint
      primotherShr <- luh2v2[, , "primn"] / setNames(totOtherLUH + 1e-10, NULL)
      secdotherShr <- luh2v2[, , "secdn"] / setNames(totOtherLUH + 1e-10, NULL)
      # where luh2 does not report other land, but we find other land in
      # CCA data, set share of secondary other land to 1
      secdotherShr[secdotherShr == 0 & primotherShr == 0] <- 1
      # multiply shares of primary and secondary non-forest veg with other land
      primother <- setNames(primotherShr, NULL) * cca[, , paste(getNames(cca, dim = 1), "other", sep = ".")]
      secdother <- setNames(secdotherShr, NULL) * cca[, , paste(getNames(cca, dim = 1), "other", sep = ".")]

      out <- mbind(
        out[, , "crop"],
        setNames(past, paste(getNames(cca, dim = 1), "past", sep = ".")),
        setNames(range, paste(getNames(cca, dim = 1), "range", sep = ".")),
        out[, , c("forestry", "primforest", "secdforest", "urban")],
        setNames(primother, paste(getNames(cca, dim = 1), "primother", sep = ".")),
        setNames(secdother, paste(getNames(cca, dim = 1), "secdother", sep = "."))
      )
    }
  } else {
    out <- cca
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
    description = paste(
      "Unprotected land area in Critical Connectivity Areas (Brennan et al. 2022)."
    ),
    isocountries = FALSE
  ))
}
