#' @title calcLivestockDistribution
#' @description Calculates gridded livestock distribution by species using GLW3/GLW4
#'   spatial snapshots (2010, 2015, 2020) linearly interpolated to requested FAO years,
#'   with constant extrapolation beyond the anchor range. Country totals come from
#'   \code{\link[mrcommons]{calcAnimalStocks}}. Eight species are covered: cattle (Ct), buffaloes
#'   (Bf), sheep (Sh), goats (Gt), horses (Ho), pigs (Pg), chickens (Ch), ducks (Dk).
#'   Horses and ducks are not available in GLW4 2020 and fall back to the 2015
#'   spatial distribution.
#'
#'   Where GLW has no pixel of a species in a country - mostly small island states, but
#'   also e.g. goats in DNK - the country's stock is spread by land area instead: by
#'   managed pasture and rangeland (or the \code{landProxy} area) for ruminants and by total
#'   land (all nine land classes) for monogastrics, and as a last resort evenly over the
#'   country's cells. Country totals of the grid therefore match the FAO stocks.
#'
#' @param output Type of output:
#'   \itemize{
#'     \item \code{"weight"}: dimensionless spatial downscaling weight within each
#'       country (sums to 1 per country per category per year).
#'     \item \code{"head"}: absolute livestock numbers (Million animals per grid cell).
#'     \item \code{"density"}: livestock per land area (Million animals per Mha).
#'       For ruminants, the denominator is the pasture/rangeland proxy area controlled
#'       by \code{landProxy}. For monogastrics (Pg, Ch, Dk), total grid-cell land area
#'       is used as the denominator.
#'   }
#' @param landProxy Land proxy controlling spatial allocation for ruminants only.
#'   Monogastrics always use GLW spatial shares regardless of this setting:
#'   \itemize{
#'     \item \code{"glw"} (default): all categories use GLW spatial shares; land data are
#'       used only for countries where GLW has no pixel of a species (see description).
#'     \item \code{"pastRange"}: ruminants (Ct, Bf, Sh, Gt, Ho) scaled by combined
#'       managed pasture and rangeland (\code{past + range}).
#'     \item \code{"speciesSpecific"}: cattle/buffalo (Ct, Bf) scaled by managed
#'       pasture (\code{past}); sheep/goats/horses (Sh, Gt, Ho) scaled by rangeland
#'       (\code{range}).
#'   }
#' @param category Livestock category classification for output:
#'   \itemize{
#'     \item \code{"FAO"}: eight FAO/GLW species (Ct, Bf, Sh, Gt, Ho, Pg, Ch, Dk).
#'     \item \code{"magpie"} (default): five MAgPIE livestock categories
#'       (livst_rum, livst_milk, livst_pig, livst_chick, livst_egg). FAO species are computed
#'       first and then aggregated using national dairy/broiler fractions from
#'       \code{\link[mrcommons]{calcAnimalStocks}}.
#'   }
#' @param selectyears Years to compute. Intersected with available FAO years.
#' @return A list with elements: \code{x} (magpie object at lpjcell resolution,
#'   67420 cells), \code{weight} (NULL), \code{unit} (character), \code{description}
#'   (character), and \code{isocountries} (FALSE — data is cellular, not country-level).
#' @author Bin Lin
#' @examples
#' \dontrun{
#' calcOutput("LivestockDistribution", output = "weight", aggregate = FALSE)
#' calcOutput("LivestockDistribution", output = "head",   aggregate = FALSE)
#' calcOutput("LivestockDistribution", output = "density", landProxy = "speciesSpecific",
#'            aggregate = FALSE)
#' calcOutput("LivestockDistribution", category = "magpie", aggregate = FALSE)
#' }
#' @importFrom magclass mbind setNames dimSums getYears getYears<- getItems time_interpolate
#' @importFrom magpiesets findset
#' @importFrom madrat calcOutput readSource
#' @importFrom mstools toolHoldConstant

calcLivestockDistribution <- function(output = "head",
                                      landProxy = "glw",
                                      category  = "magpie",
                                      selectyears = paste0("y", 1961:2025)) {

  if (!output %in% c("weight", "head", "density")) {
    stop("output must be one of 'weight', 'head', or 'density'.")
  }
  if (!landProxy %in% c("glw", "pastRange", "speciesSpecific")) {
    stop("landProxy must be one of 'glw', 'pastRange', or 'speciesSpecific'.")
  }
  if (!category %in% c("FAO", "magpie")) {
    stop("category must be either 'FAO' or 'magpie'.")
  }

  # expand any named sets (e.g. "past"); keep explicit year strings (e.g. "y2011") as-is
  isYearStr <- function(s) grepl("^y[0-9]{4}$", s)
  selectyears <- sort(unique(unlist(lapply(selectyears, function(s) {
    if (isYearStr(s)) s else findset(s, noset = "original")
  }))))

  speciesAll      <- c("Ct", "Bf", "Sh", "Gt", "Ho", "Pg", "Ch", "Dk")
  speciesRuminant <- c("Ct", "Bf", "Sh", "Gt", "Ho")

  # read GLW spatial snapshots (heads/pixel); Ho and Dk missing in GLW4 2020, fall back to 2015
  glw2010 <- mbind(lapply(speciesAll, function(sp) {
    setNames(readSource("GLW3all", subtype = paste0("Da_", sp, "_2010")), sp)
  }))

  glw2015 <- mbind(lapply(speciesAll, function(sp) {
    setNames(readSource("GLW4", subtype = paste0("Da_", sp, "_2015")), sp)
  }))

  species2020    <- c("Ct", "Bf", "Sh", "Gt", "Pg", "Ch")
  glw2020        <- glw2015
  getYears(glw2020) <- "y2020"
  glw2020Partial <- mbind(lapply(species2020, function(sp) {
    x <- readSource("GLW4", subtype = paste0("Da_", sp, "_2020"))
    getYears(x) <- "y2020"
    setNames(x, sp)
  }))
  glw2020[, , species2020] <- glw2020Partial

  glw <- mbind(glw2010, glw2015, glw2020)
  glw[is.na(glw) | glw < 0] <- 0

  isoPerCell <- getItems(glw, dim = "iso", full = TRUE)

  # aggregate IPCC categories from calcAnimalStocks to GLW species
  animalStocks <- calcOutput("AnimalStocks", aggregate = FALSE)

  faoStocks <- mbind(
    setNames(dimSums(animalStocks[, , c("dairy cows",     "other cattle")],   dim = 3), "Ct"),
    setNames(dimSums(animalStocks[, , c("dairy buffalo",  "other buffalo")],  dim = 3), "Bf"),
    setNames(dimSums(animalStocks[, , c("dairy sheep",    "other sheep")],    dim = 3), "Sh"),
    setNames(dimSums(animalStocks[, , c("dairy goats",    "other goats")],    dim = 3), "Gt"),
    setNames(animalStocks[, , "horses"],                                                "Ho"),
    setNames(dimSums(animalStocks[, , c("market swine",   "breeding swine")], dim = 3), "Pg"),
    setNames(dimSums(animalStocks[, , c("poultry layers", "broilers")],       dim = 3), "Ch"),
    setNames(animalStocks[, , "ducks"],                                                 "Dk")
  )
  faoStocks[is.na(faoStocks) | faoStocks < 0] <- 0
  faoStocks[faoStocks == 0] <- 1e-6 # 1 animal

  availYears <- getYears(faoStocks)
  beyondFAO  <- selectyears[selectyears > max(availYears)]
  if (length(beyondFAO) > 0) {
    faoStocks <- toolHoldConstant(faoStocks, years = beyondFAO)
  }
  selectyears <- intersect(selectyears, getYears(faoStocks))
  if (length(selectyears) == 0) stop("No requested years available in FAO data.")
  faoStocks <- faoStocks[, selectyears, ]
  faoYears  <- getYears(faoStocks, as.integer = TRUE)

  land      <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell",
                          nclasses = "nine", aggregate = FALSE)
  land[is.na(land) | land < 0] <- 0
  landYears   <- getYears(land, as.integer = TRUE)

  # expand country-level data to grid, preserving glw's full spatial structure
  expandToGrid <- function(countryData, yrStr, isoVec) {
    out <- glw[, "y2010", ]
    getYears(out) <- yrStr
    out[, , ] <- as.numeric(countryData[isoVec, yrStr, ])
    return(out)
  }

  # normalize so that grid values sum to 1 within each country;
  # uses tapply to avoid magclass dimensional algebra across different dim3 set names
  normalizeWithinCountry <- function(x, isoVec) {
    nCells <- dim(x)[1]
    xVec   <- as.numeric(x)
    nSlice <- length(xVec) / nCells
    outVec <- numeric(length(xVec))
    isoFac <- factor(isoVec)
    for (j in seq_len(nSlice)) {
      idx  <- seq_len(nCells) + (j - 1L) * nCells
      vec  <- xVec[idx]
      csum <- tapply(vec, isoFac, sum, na.rm = TRUE)
      cvec <- as.numeric(csum[isoFac])
      ok   <- !is.na(cvec) & cvec > 0
      outVec[idx[ok]] <- vec[ok] / cvec[ok]
    }
    out <- x
    out[, , ] <- outVec
    return(out)
  }

  # uniform share (1/n cells) within each country - last-resort fallback used only when
  # no spatial information (GLW, land proxy, or land at all) exists anywhere for that
  # country, e.g. small Pacific/Indian Ocean island states with literally no land-use
  # data recorded at 0.5 degree resolution (see @description).
  uniformWithinCountry <- function(x, isoVec) {
    nCells <- dim(x)[1]
    nSlice <- length(as.numeric(x)) / nCells
    isoFac <- factor(isoVec)
    nCellsPerCtry <- as.numeric(table(isoFac))
    names(nCellsPerCtry) <- levels(isoFac)
    perCellShare <- 1 / nCellsPerCtry[isoVec]
    out <- x
    out[, , ] <- rep(perCellShare, nSlice)
    out
  }

  # for cells where GLW = 0 but land proxy > 0, fill with country mean density;
  # applied once per GLW anchor year (2010/2015/2020, see densityAnchors below) so the
  # gap-fill uses that anchor's own real land proxy, not a placeholder.
  # All arithmetic uses as.numeric() to avoid magclass outer-product when dim3
  # set names differ between GLW (species) and land-area objects.
  fillGLWZeros <- function(baseGLW, cellArea, landYr, isoVec) {
    glwArr  <- as.numeric(baseGLW)
    areaArr <- as.numeric(cellArea)
    glwDensity <- baseGLW
    glwDensity[, , ] <- ifelse(areaArr > 0, glwArr / areaArr, 0)

    positive     <- glwArr > 0
    ctryHeads    <- dimSums(baseGLW, dim = c("x", "y"))

    positiveArea <- cellArea
    positiveArea[!positive] <- 0
    positiveArea[is.na(positiveArea)] <- 0
    ctryArea <- dimSums(positiveArea, dim = c("x", "y"))
    ctryArea[ctryArea <= 0] <- NA

    ctryMeanDensity <- ctryHeads
    ctryMeanDensity[, , ] <- as.numeric(ctryHeads) / as.numeric(ctryArea)

    ctryMeanGrid <- baseGLW
    ctryMeanGrid[, , ] <- as.numeric(ctryMeanDensity[isoVec, , ])

    zeroCells <- glwArr == 0 & as.numeric(landYr) > 0
    glwDensity[zeroCells] <- ctryMeanGrid[zeroCells]
    glwDensity[is.na(glwDensity)] <- 0
    return(glwDensity)
  }

  # if country total of primary is zero, fall back to land proxy then raw GLW shares;
  # if even that is zero (no spatial information anywhere for that country), split
  # evenly across the country's own cells as a last resort - see uniformWithinCountry()
  withFallback <- function(primary, fallback1, fallback2, isoVec) {
    share    <- normalizeWithinCountry(primary,   isoVec)
    primCtry <- dimSums(primary,   dim = c("x", "y"))
    fb1Ctry  <- dimSums(fallback1, dim = c("x", "y"))
    fb2Ctry  <- dimSums(fallback2, dim = c("x", "y"))

    expandFlag <- function(flag) {
      g <- primary
      g[, , ] <- as.numeric(flag[isoVec, , ])
      g
    }

    # blend fallback shares via plain numeric vectors to avoid dim3 mismatch
    shareArr <- as.numeric(share)
    fb1Share <- normalizeWithinCountry(fallback1, isoVec)
    pz  <- as.numeric(expandFlag(primCtry == 0)) > 0
    shareArr[pz] <- as.numeric(fb1Share)[pz]

    fb2Share <- normalizeWithinCountry(fallback2, isoVec)
    fb2 <- as.numeric(expandFlag(primCtry == 0 & fb1Ctry == 0)) > 0
    shareArr[fb2] <- as.numeric(fb2Share)[fb2]

    # last resort: GLW, land proxy and the final fallback are all zero for this country
    # (e.g. small island states with no land-use data recorded at all at 0.5 degree
    # resolution) - an uninformed uniform guess is still far better than the real FAO
    # total silently vanishing (leaving only the rounding epsilon added at the very end
    # of calcLivestockDistribution).
    fb3Share <- uniformWithinCountry(primary, isoVec)
    fb3 <- as.numeric(expandFlag(primCtry == 0 & fb1Ctry == 0 & fb2Ctry == 0)) > 0
    shareArr[fb3] <- as.numeric(fb3Share)[fb3]

    share[, , ] <- shareArr
    share[is.na(share) | is.nan(share) | is.infinite(share)] <- 0
    share
  }

  # linearly interpolate GLW snapshots to FAO years; constant extrapolation outside anchors
  # integrate_interpolated_years = FALSE because GLW is a stock variable, not a flow
  glwInterp <- time_interpolate(glw,
                                interpolated_year            = paste0("y", faoYears),
                                integrate_interpolated_years = FALSE,
                                extrapolation_type           = "constant")

  # land-adjusted ruminants: compute a land-proxy density at each real GLW anchor year
  # (2010/2015/2020), paired with that same year's land proxy area, then interpolate the
  # density itself over time (same scheme as glwInterp above) so it exactly reproduces
  # GLW at every anchor and only drifts from it in proportion to how the land proxy
  # itself changes in other years.
  if (landProxy != "glw") {
    anchorYrs      <- c(2010, 2015, 2020)
    densityAnchors <- mbind(lapply(anchorYrs, function(ay) {
      ayStr     <- paste0("y", ay)
      landAyStr <- nearestLandYear(ay, landYears)
      mbind(lapply(speciesRuminant, function(sp) {
        proxyAy <- getLandProxy(sp, landAyStr, land, landProxy)
        getYears(proxyAy) <- ayStr
        d <- fillGLWZeros(glw[, ayStr, sp], proxyAy, proxyAy, isoPerCell)
        setNames(d, sp)
      }))
    }))

    densityInterp <- time_interpolate(densityAnchors,
                                      interpolated_year            = paste0("y", faoYears),
                                      integrate_interpolated_years = FALSE,
                                      extrapolation_type           = "constant")
  }

  headsList <- lapply(faoYears, function(yr) {

    yrStr   <- paste0("y", yr)
    faoGrid <- expandToGrid(faoStocks, yrStr, isoPerCell)

    speciesOut <- lapply(speciesAll, function(sp) {

      if (sp %in% speciesRuminant && landProxy != "glw") {

        landYrStr <- if (yrStr %in% getYears(land)) yrStr else nearestLandYear(yr, landYears)
        landYr    <- getLandProxy(sp, landYrStr, land, landProxy)
        getYears(landYr) <- yrStr

        baseGLW <- glwInterp[, yrStr, sp]
        dens    <- densityInterp[, yrStr, sp]

        # weight = interpolated land-proxy density (see densityAnchors/densityInterp
        # above) * this year's real land proxy area, with country fallback if the
        # country total is zero; rawAlloc and outSp use baseGLW as template to keep
        # species dim3 structure
        rawAlloc <- baseGLW
        rawAlloc[, , ] <- as.numeric(dens) * as.numeric(landYr)
        getYears(rawAlloc) <- yrStr
        share <- withFallback(rawAlloc, landYr, baseGLW, isoPerCell)
        outSp <- share
        outSp[, , ] <- as.numeric(share) * as.numeric(faoGrid[, yrStr, sp])

      } else if (sp %in% speciesRuminant) {
        # landProxy == "glw": ruminants use GLW spatial shares; countries without a GLW
        # pixel of the species fall back to managed pasture + rangeland, then total land
        landYrStr <- if (yrStr %in% getYears(land)) yrStr else nearestLandYear(yr, landYears)
        pastRange <- land[, landYrStr, "past"] + land[, landYrStr, "range"]
        getYears(pastRange) <- yrStr
        totalLand <- dimSums(land[, landYrStr, ], dim = 3)
        getYears(totalLand) <- yrStr

        share <- withFallback(glwInterp[, yrStr, sp], pastRange, totalLand, isoPerCell)
        outSp <- share
        outSp[, , ] <- as.numeric(share) * as.numeric(faoGrid[, yrStr, sp])

      } else {
        # monogastrics (Pg, Ch, Dk): primary = raw GLW shares. GLW has zero grid signal
        # for ~70 countries per product (mostly small island states with no satellite
        # pixel coverage); without a fallback their FAO stock silently vanishes from the
        # grid instead of just being misallocated. Fall back to total land area (all nine
        # classes summed), spreading the country's stock across whatever land it has
        # rather than concentrating it into a single land class.
        landYrStr <- if (yrStr %in% getYears(land)) yrStr else nearestLandYear(yr, landYears)
        totalLand <- dimSums(land[, landYrStr, ], dim = 3)
        getYears(totalLand) <- yrStr

        baseGLW <- glwInterp[, yrStr, sp]
        share   <- withFallback(baseGLW, totalLand, totalLand, isoPerCell)
        outSp   <- share
        outSp[, , ] <- as.numeric(share) * as.numeric(faoGrid[, yrStr, sp])
      }

      setNames(outSp, sp)
    })

    mbind(speciesOut)
  })

  heads <- mbind(headsList)
  heads[is.na(heads) | heads < 0] <- 0

  # aggregate 8 FAO species to 5 MAgPIE kli categories
  if (category == "magpie") {
    heads <- aggregateToMagpieKli(heads, animalStocks, isoPerCell)
    speciesAll      <- c("livst_rum", "livst_milk", "livst_pig", "livst_chick", "livst_egg")
    speciesRuminant <- c("livst_rum", "livst_milk")
  }

  if (output == "head") {

    out  <- heads
    unit <- "Million animals"

  } else if (output == "weight") {

    out  <- normalizeWithinCountry(heads, isoPerCell)
    unit <- "1"

  } else {
    # output == "density"; extrapolate land to head years so no years are dropped
    landDens <- time_interpolate(land,
                                 interpolated_year            = getYears(heads),
                                 integrate_interpolated_years = FALSE,
                                 extrapolation_type           = "constant")

    densityList <- lapply(speciesAll, function(sp) {
      denom <- if (sp %in% speciesRuminant) {
        if (category == "magpie" || landProxy %in% c("glw", "pastRange")) {
          landDens[, , "past"] + landDens[, , "range"]
        } else {
          if (sp %in% c("Ct", "Bf")) landDens[, , "past"] else landDens[, , "range"]
        }
      } else {
        dimSums(landDens, dim = 3)
      }
      denom[as.numeric(denom) < 1e-6] <- NA
      dens <- heads[, , sp]
      dens[, , ] <- as.numeric(heads[, , sp]) / as.numeric(denom)
      dens[!is.finite(as.numeric(dens))] <- 0
      setNames(dens, sp)
    })

    out  <- mbind(densityList)
    unit <- "Million animals per Mha"
  }

  # round to a sane file-size precision, with a tiny epsilon so genuinely non-zero cells
  # never round down to a literal stored zero. Only cells that are actually non-zero are
  # bumped - genuine zeros stay zero, so empty areas (e.g. the entire Arctic) are not
  # seeded with a spurious 1e-6 weight that a region-level GLW disaggregation would then
  # smear livestock into. Runs after all fallback/normalisation logic above, so it doesn't
  # affect any of the country-sum-is-zero checks.
  zeroCells <- out == 0
  out <- round(out + 1e-6, 6)
  out[zeroCells] <- 0

  return(list(
    x            = out,
    weight       = NULL,
    unit         = unit,
    description  = paste0("Gridded livestock (GLW3/4 spatial anchors, FAO time series). ",
                          "output='", output, "', landProxy='", landProxy,
                          "', category='", category, "'"),
    isocountries = FALSE
  ))
}

#' Land proxy (pasture/rangeland area) for a given species and land-use year,
#' controlled by the landProxy setting ("pastRange" combines both classes;
#' otherwise cattle/buffalo use pasture and sheep/goats/horses use rangeland).
#' @noRd
getLandProxy <- function(sp, yrs, land, landProxy) {
  if (landProxy == "pastRange") {
    return(land[, yrs, "past"] + land[, yrs, "range"])
  }
  if (sp %in% c("Ct", "Bf")) return(land[, yrs, "past"])
  if (sp %in% c("Sh", "Gt", "Ho")) return(land[, yrs, "range"])
  stop("No land proxy defined for species ", sp, " with landProxy '", landProxy, "'")
}

#' Nearest available land-use year to a given target year.
#' @noRd
nearestLandYear <- function(yr, landYears) {
  paste0("y", landYears[which.min(abs(landYears - yr))])
}

#' Aggregate 8 FAO/GLW species to 5 MAgPIE kli categories using national
#' dairy-cow/-buffalo/-sheep/-goat and egg-layer fractions from calcAnimalStocks.
#' @noRd
aggregateToMagpieKli <- function(heads, animalStocks, isoPerCell) {
  yrs    <- getYears(heads)
  astExt <- setdiff(yrs, getYears(animalStocks))
  if (length(astExt) > 0) animalStocks <- toolHoldConstant(animalStocks, years = astExt)
  ast <- animalStocks[, yrs, ]

  safeFrac <- function(num, den) {
    f <- num
    f[, , ] <- ifelse(as.numeric(den) > 0, as.numeric(num) / as.numeric(den), 0)
    f
  }

  dairyCowFrac   <- safeFrac(ast[, , "dairy cows"],
                             dimSums(ast[, , c("dairy cows", "other cattle")], dim = 3))
  dairyBufFrac   <- safeFrac(ast[, , "dairy buffalo"],
                             dimSums(ast[, , c("dairy buffalo", "other buffalo")], dim = 3))
  dairySheepFrac <- safeFrac(ast[, , "dairy sheep"],
                             dimSums(ast[, , c("dairy sheep", "other sheep")], dim = 3))
  dairyGoatFrac  <- safeFrac(ast[, , "dairy goats"],
                             dimSums(ast[, , c("dairy goats", "other goats")], dim = 3))
  layerFrac      <- safeFrac(ast[, , "poultry layers"],
                             dimSums(ast[, , c("poultry layers", "broilers")], dim = 3))

  expandFrac <- function(frac) {
    g <- heads[, , "Ct"]
    g[, , ] <- as.numeric(frac[isoPerCell, , ])
    g
  }

  fCt <- as.numeric(expandFrac(dairyCowFrac))
  fBf <- as.numeric(expandFrac(dairyBufFrac))
  fSh <- as.numeric(expandFrac(dairySheepFrac))
  fGt <- as.numeric(expandFrac(dairyGoatFrac))
  fCh <- as.numeric(expandFrac(layerFrac))

  nCt <- as.numeric(heads[, , "Ct"])
  nBf <- as.numeric(heads[, , "Bf"])
  nSh <- as.numeric(heads[, , "Sh"])
  nGt <- as.numeric(heads[, , "Gt"])
  nHo <- as.numeric(heads[, , "Ho"])
  nPg <- as.numeric(heads[, , "Pg"])
  nCh <- as.numeric(heads[, , "Ch"])
  nDk <- as.numeric(heads[, , "Dk"])

  tmpl <- heads[, , "Ct"]
  mkKli <- function(vals, name) {
    x <- tmpl
    x[, , ] <- vals
    setNames(x, name)
  }

  out <- mbind(
    mkKli((1 - fCt) * nCt + (1 - fBf) * nBf + (1 - fSh) * nSh + (1 - fGt) * nGt + nHo, "livst_rum"),
    mkKli(fCt * nCt + fBf * nBf + fSh * nSh + fGt * nGt,                              "livst_milk"),
    mkKli(nPg,                                                                        "livst_pig"),
    mkKli((1 - fCh) * nCh + nDk,                                                      "livst_chick"),
    mkKli(fCh * nCh,                                                                  "livst_egg")
  )
  out[is.na(out) | out < 0] <- 0
  out
}
