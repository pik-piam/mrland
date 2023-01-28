#' @title calcConservationPriority2
#' @description Function calculates land area in conservation priority areas that was unprotected in 2020 (WDPA).
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
  # KBAs have global value for conservation, due to their
  # outstanding ecological integrity, globally important ecosystems
  # or significant populations of animals, fungi and plants.
  # https://www.iucn.org/resources/conservation-tool/key-biodiversity-areas

  kba <- calcOutput("KeyBiodiversityAreas",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # ---------------------------
  # Global Safety Net (GSN)
  # ---------------------------
  # Areas of the terrestrial realm where increased
  # conservation action is needed to protect biodiversity
  # and store carbon (Dinerstein et al. 2020, Science).

  gsn <- calcOutput("GlobalSafetyNet",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )
  # Unprotected Key Biodiversity Areas have been masked at
  # high resolution to remove overlaps and can be now
  # added up
  gsn <- gsn + kba
  gsn <- collapseDim(gsn, dim = 3.3)

  # ----------------------------------
  # Critical Connectivity Areas (CCA)
  # ----------------------------------
  # Brennan et al. (2022), Science:
  # "Areas where the flow of animal movement
  # is concentrated are places with the potential to
  # disproportionately reduce connectivity if further
  # restricted or destroyed [...]"

  # Critical Connectivity Areas relate to th upper
  # 90th percentile of values of mapped
  # mammal movement probabilities

  cca <- calcOutput("CriticalConnectivityAreas",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, mask = "KBA", aggregate = FALSE
  )

  # Unprotected Key Biodiversity Areas have been masked at
  # high resolution to remove overlaps and can be now
  # combined with KBAs to create a balanced conservation
  # template
  cca <- cca + kba
  cca <- collapseDim(cca, dim = 3.3)

  # ------------------------------
  # Irrecoverable carbon
  # ------------------------------
  # Noon et al. (2022), Nature Sustainability:
  # "Irrecoverable carbon reserves are [...] manageable, vulnerable to
  # disturbance and could not be recovered by 2050 if lost today.
  # While irrecoverabilitycan be considered over any timeframe, we
  # selected 30 years as the most policy-relevant scenario to align with
  # the Paris Agreement goal to reach net-zero emissions by mid-century."

  irrC <- calcOutput("IrrecoverableCarbonLand",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # -------------------------------------------------
  # Biodiversity Hotspots & Intact Forest Landscapes
  # -------------------------------------------------
  # Biodiversity Hotspots have at least 1500 vascular plants as endemics
  # and must have 30 % or less of its original natural vegetation.
  # https://www.conservation.org/priorities/biodiversity-hotspots

  # An intact forest landscape (IFL) is a seamless mosaic of forest and
  # naturally treeless ecosystems with no remotely detected signs of
  # human activity and a minimum area of 500 km2 (Potapov et al. 2017,
  # Science Advances)

  bhifl <- calcOutput("BHIFL",
    nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  # -------------------------------------------------
  # Half Earth (PBL)
  # -------------------------------------------------
  # An ecoregion-based approach to protecting half of
  # the terrestrial surface. 50 % of land are conserved in
  # each to the 867 ecoregions.

  pblHalfEarth <- calcOutput("HalfEarth",
    nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  getNames(pblHalfEarth) <- paste0("PBL_", getNames(pblHalfEarth))

  # ----------------------------
  # 30 % of land surface
  # ----------------------------
  # A scenario for the 30 by 30 target that is based on
  # Key Biodiversity Areas, Distinct Species Assemblages (DSA)
  # from the Global Safety Net and Critical Connectivity Areas (CCAs).

  gsn30 <- calcOutput("GlobalSafetyNet",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )
  # Use GSN Distinct Species Assemblages (DSA) for the 30 % target
  gsn30 <- gsn30[, , "GSN_DSA"]

  # To remove overlaps CCAs have already been masked
  # by KBAs and GSN DSAs at high resolution
  cca30 <- calcOutput("CriticalConnectivityAreas",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, mask = "KBA_GSN", aggregate = FALSE
  )

  # combine KBA, GSN & CCA data sets
  thirty <- mbind(kba, gsn30, cca30)
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
  # The sum of all conservation clusters of the GSN
  # pertains to 50.4 % of the global terrestrial realm
  # together with currently protected areas.

  gsnHalfEarth <- calcOutput("GlobalSafetyNet",
    maginput = TRUE, nclasses = nclasses,
    cells = cells, aggregate = FALSE
  )

  gsnHalfEarth <- mbind(kba, gsnHalfEarth)
  gsnHalfEarth <- dimSums(gsnHalfEarth, dim = 3.1)
  getNames(gsnHalfEarth) <- paste("GSN_HalfEarth", getNames(gsnHalfEarth), sep = ".")

  # ---------------------------------------
  # Bind all conservation priority areas
  # ---------------------------------------
  # Combine all templates for the output
  cp <- mbind(thirty, kba, gsn, bhifl, irrC, cca, gsnHalfEarth, pblHalfEarth)

  return(list(
    x = cp,
    weight = NULL,
    unit = "Mha",
    description = "Land conservation priority targets in each land type",
    isocountries = FALSE
  ))
}
