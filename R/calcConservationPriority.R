#' @title calcConservationPriority
#' @description Function calculates land area in conservation priority areas
#'
#' @param cells number of cells of landmask (select "magpiecell" for 59199 cells or "lpjcell" for 67420 cells)
#' @param nclasses Options are either "seven" or "nine".
#' \itemize{
#' \item "seven" separates primary and secondary forest and includes "crop", "past", "forestry", "primforest", "secdforest", "urban" and "other"
#' \item "nine" adds the separation of pasture and rangelands, as well as a differentiation of primary and secondary non-forest vegetation and therefore returns "crop", "past", "range", "forestry", "primforest", "secdforest", "urban", "primother" and "secdother"
#' }
#'
#' @return magpie object in cellular resolution with different protection options in conservation priority areas
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("ConservationPriority", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset addLocation
#' @importFrom magclass collapseDim
#' @importFrom mrcommons toolCoord2Isocell
#'

calcConservationPriority <- function(cells = "magpiecell", nclasses = "seven") {

  # Land area (in Mha):
  LUini <- calcOutput("LanduseInitialisation",
    cellular = TRUE, cells = cells,
    nclasses = nclasses, fao_corr = TRUE, input_magpie = TRUE,
    years = "y1995", aggregate = FALSE
  )
  landArea <- dimSums(LUini, dim = 3)

  # Protection Area mz file (conservation priority area in Mha)
  x <- readSource("ProtectArea", convert = "onlycorrect")

  ### Rename items:
  # In this data set, FF stands for intact forest landscapes (IFL)
  getNames(x) <- gsub("FF", "IFL", getNames(x))
  getNames(x) <- gsub("WDPA", "None", getNames(x))

  # Create dummy
  x[, , "None"] <- 0

  # Add BH_IFL scenario (combination of biodiversity hotspots and intact forestry landscapes)
  # (Note: should only be applied to forests (for other land, use BH))
  bhifl <- setNames(pmax(x[, , "BH"], x[, , "IFL"]), nm = "BH_IFL")
  x <- mbind(x, bhifl)

  # Half Earth Protection Share
  HalfEarthShr <- readSource("HalfEarth", convert = "onlycorrect")
  getNames(HalfEarthShr) <- "HalfEarth"
  getSets(HalfEarthShr) <- c("x", "y", "iso", "year", "data")

  if (cells == "magpiecell") {
    HalfEarthShr <- toolCoord2Isocell(HalfEarthShr, cells = cells)
  } else if (cells == "lpjcell") {
    landArea <- collapseDim(addLocation(landArea), dim = c("N", "region"))
    tmp <- collapseDim(addLocation(x), dim = c("region", "cell"))
    x <- new.magpie(
      cells_and_regions = getCells(collapseDim(HalfEarthShr, dim = "iso")),
      years = getYears(tmp),
      names = getNames(tmp), fill = 0,
      sets = c("x.y.iso", "year", "data")
    )
    x[getCells(tmp), , ] <- tmp
    map <- toolGetMappingCoord2Country()
    if (any(getCells(x) != map$coords)) {
      stop("Wrong cell ordering in calcConservationPriority")
    }
    getCells(x) <- paste(map$coords, map$iso, sep = ".")
  } else {
    stop("Please select magpiecell or lpjcell in cells argument of calcConservationPriority")
  }

  # Land area to be protected by 2050 (in Mha)
  HalfEarthArea <- HalfEarthShr * landArea

  # Add HalfEarth scenario to Protection area data
  x <- mbind(x, HalfEarthArea)

  # get WDPA baseline data (1995 - 2020)
  wdpaBase <- calcOutput("ProtectedAreaBaseline",
    aggregate = FALSE, cells = cells,
    nclasses = nclasses, magpie_input = TRUE
  )

  # make sure that current WDPA protected areas
  # are not part of conservation priority targets
  x <- x - dimSums(wdpaBase[, "y2020", ], dim = 3)
  x <- toolConditionalReplace(x, "<0", 0)

  urbanLand <- calcOutput("UrbanLandFuture", subtype = "LUH2v2", aggregate = FALSE,
                          timestep = "5year", cells = cells)

  # Conservation potential after 2020
  consvPot <- landArea - dimSums(wdpaBase[, "y2020", ], dim = 3) - setCells(urbanLand[, "y2020", "SSP2"], getCells(wdpaBase))
  consvPot <- toolConditionalReplace(consvPot, "<0", 0)

  # Where conservation priority area is smaller than conservation potential
  # replace by conservation potential
  for (c in getNames(x)) {
    tmp <- x[, , c]
    tmp[tmp > consvPot] <- consvPot[tmp > consvPot]
    x[, , c] <- tmp
  }

  if (nclasses == "seven") {
    # calulate share of respective natveg classes
    NatvegShr <- LUini[, , c("primforest", "secdforest", "other")] / dimSums(LUini[, , c("primforest", "secdforest", "other")], dim = 3)
    NatvegShr <- toolConditionalReplace(NatvegShr, "is.na()", 0)

    # magpie object containing all land classes and their conservation land shares
    LandShr <- mbind(
      new.magpie(getCells(NatvegShr), NULL, c("crop", "past", "forestry"), fill = 0),
      NatvegShr[, , c("primforest", "secdforest")],
      new.magpie(getCells(NatvegShr), NULL, "urban", fill = 0),
      NatvegShr[, , "other"]
    )
  } else if (nclasses == "nine") {
    # calulate share of respective natveg classes
    NatvegShr <- LUini[, , c("primforest", "secdforest", "primother", "secdother")] / dimSums(LUini[, , c("primforest", "secdforest", "primother", "secdother")], dim = 3)
    NatvegShr <- toolConditionalReplace(NatvegShr, "is.na()", 0)

    # magpie object containing all land classes and their conservation land shares
    LandShr <- mbind(
      new.magpie(getCells(NatvegShr), NULL, c("crop", "past", "forestry"), fill = 0),
      NatvegShr[, , c("primforest", "secdforest")],
      new.magpie(getCells(NatvegShr), NULL, "urban", fill = 0),
      NatvegShr[, , "primother"],
      NatvegShr[, , "secdother"]
    )
  }

  # compute land area reserved for conservation
  x <- x * setCells(LandShr, getCells(x))

  return(list(
    x = x,
    weight = NULL,
    unit = "Mha",
    description = "Land conservation priority area (BH, IFL, CPD, LW & BH_IFL) in each land type",
    isocountries = FALSE
  ))
}
