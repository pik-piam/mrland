#' @title calcConservationPriority2
#' @description Function calculates land area in conservation priority areas
#'
#' @param cells number of cells of landmask (select "magpiecell" for 59199 cells or "lpjcell" for 67420 cells)
#' @param nclasses Options are either "seven" or "nine".
#' \itemize{
#' \item "seven" separates primary and secondary forest and includes "crop", "past", "forestry",
#' "primforest", "secdforest", "urban" and "other"
#' \item "nine" adds the separation of pasture and rangelands, as well as a differentiation of
#' primary and secondary non-forest vegetation and therefore returns "crop", "past", "range",
#' "forestry", "primforest", "secdforest", "urban", "primother" and "secdother"
#' }
#'
#' @return magpie object in cellular resolution with different protection options in conservation priority areas
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("ConservationPriority2", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset addLocation
#' @importFrom magclass collapseDim
#' @importFrom mrcommons toolCoord2Isocell
#'

calcConservationPriority2 <- function(cells = "magpiecell", nclasses = "seven") {

  # ------------------------------
  # Key Biodiversity Areas (KBAs)
  # ------------------------------
  kba <- calcOutput("KeyBiodiversityAreas",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # ---------------------------
  # Global Safety Net (GSN)
  # ---------------------------
  gsn <- calcOutput("GlobalSafetyNet",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # ----------------------------------
  # Critical Connectivity Areas (CCA)
  # ----------------------------------
  cca <- calcOutput("CriticalConnectivityAreas",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # ------------------------------
  # Irrecoverable carbon
  # ------------------------------
  irrC <- calcOutput("IrrecoverableCarbonLand",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # -------------------------------------------------
  # Biodiversity Hotspots & Intact Forest Landscapes
  # -------------------------------------------------
  bhifl <- calcOutput("BHIFL",
    nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # -------------------------------------------------
  # Half Earth (PBL)
  # -------------------------------------------------
  pblHalfEarth <- calcOutput("HalfEarth",
    nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  getNames(pblHalfEarth) <- paste0("PBL_", getNames(pblHalfEarth))

  # ----------------------------
  # 30 % of land surface
  # ----------------------------
  # Based on Key Biodiversity Areas, Distinct Species Assemblages (DSA)
  # from the Global Safety Net and Critical Connectivity Areas (CCAs).

  # combine data sets
  thirty <- mbind(kba, gsn[, , "GSN_DSA"], cca)
  thirty <- dimSums(thirty, dim = 3.1)
  getNames(thirty) <- paste("30by30", getNames(thirty), sep = ".")

  # Unprotected KBAs, DSAs and CCAs make up 16.13 %
  # of the land surface. In 2020 ~15 % of global
  # land surface was protected (WDPA). Therefore
  # the conservation target is rescaled to 15 % to sum
  # up to to 30 % together with WDPA protected areas.

  # Land area (in Mha):
  luIni <- calcOutput("LanduseInitialisation",
    cellular = TRUE, cells = cells,
    nclasses = nclasses, input_magpie = TRUE,
    years = "y1995", aggregate = FALSE
  )
  gloLandArea <- dimSums(luIni, dim = c(1, 3))

  # get scaling factor
  target <- gloLandArea * 0.15
  thirtySum <- dimSums(thirty, c(1, 3))
  scalingFactor <- target / thirtySum

  # KBAs, DSAs and CCAs are rescaled from ~16 % to 15 %
  thirty <- thirty * scalingFactor

  # --------------------------------------------------
  # Half Earth (Global Safety Net)
  # --------------------------------------------------

  gsnHalfEarth <- mbind(kba, gsn)
  gsnHalfEarth <- dimSums(gsnHalfEarth, dim = 3.1)
  getNames(gsnHalfEarth) <- paste("GSN_HalfEarth", getNames(gsnHalfEarth), sep = ".")

  # ---------------------------------------
  # Bind all conservation priority areas
  # ---------------------------------------

  cp <- mbind(thirty, kba, gsn, bhifl, irrC, cca, gsnHalfEarth, pblHalfEarth)

  return(list(
    x = cp,
    weight = NULL,
    unit = "Mha",
    description = "Land conservation priority targets in each land type",
    isocountries = FALSE
  ))
}
