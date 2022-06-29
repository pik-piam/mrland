#' @title calcAvlCropland
#'
#' @description Calculates the total available cropland per grid cell,
#'              based on physical cropland suitability data or other criteria,
#'              such as constraints on cropland expansion
#'
#' @param marginal_land  Defines which share of marginal land should be included (see options below) and
#'                whether suitable land under irrigated conditions ("irrigated"), under rainfed conditions ("rainfed")
#'                or suitability under rainfed conditions including currently irrigated land (rainfed_and_irrigated)
#'                should be used. Options combined via ":"
#'                The different marginal land options are:
#' \itemize{
#' \item \code{"all_marginal"}: Of the total marginal land (suitability index = 0.0 - 0.33),
#' areas with an index of 0.1 and lower are excluded.
#' \item \code{"q33_marginal"}: The bottom tertile (suitability index below 0.13) of the
#' marginal land area is excluded.
#' \item \code{"q50_marginal"}: The bottom  half (suitability index below 0.18) of the
#' marginal land area is excluded.
#' \item \code{"q66_marginal"}: The first and second tertile (suitability index below 0.23) of
#' the marginal land area are excluded.
#' \item \code{"q75_marginal"}: The first, second and third quartiles (suitability index below 0.25)
#' of the marginal land are are excluded
#' \item \code{"no_marginal"}: Areas with a suitability index of 0.33 and lower are excluded.
#' \item \code{"magpie"}: Returns "all_marginal:rainfed_and_irrigated",
#'                        "q33_marginal:rainfed_and_irrigated" and
#'                        "no_marginal:rainfed_and_irrigated"
#'                        in a magclass object to be used as magpie input.
#' }
#' @param cell_upper_bound Upper bound for cropland at the grid cell level.
#'                         Even if, for instance, the total available cropland area equals the land area in a grid cell,
#'                         cropland cannot be expanded above this value.
#' @param cells            magpiecell (59199 cells) or lpjcell (67420 cells)
#' @param country_level    Whether output shall be at country level.
#'                         Requires aggregate=FALSE in calcOutput.
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("AvlCropland", marginal_land = "magpie", cells = "magpiecell",
#'            country_level = FALSE, aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass dimSums getCells getYears getNames mbind collapseDim as.magpie
#' @importFrom magpiesets addLocation
#' @importFrom mrcommons toolCoord2Isocell toolAggregateCell2Country
#'

calcAvlCropland <- function(marginal_land = "magpie", cell_upper_bound = 0.9,
                            cells = "magpiecell", country_level = FALSE) {

  # read luh data
  luh <- calcOutput("LUH2v2", landuse_types = "magpie", aggregate = FALSE,
                    cellular = TRUE, cells = "lpjcell", irrigation = FALSE, years = "y1995")
  # sum land area per grid cell
  luh      <- collapseDim(addLocation(luh), dim = c("N", "cell"))
  landarea <- dimSums(luh, dim = 3)

  x <- as.magpie(NULL)

  if (any(grepl("all_marginal", marginal_land)) || marginal_land == "magpie") {

    if (marginal_land == "magpie") {
      cropsuit <- readSource("Zabel2014", subtype = paste("all_marginal", "rainfed_and_irrigated",
                                                          sep = ":"),
                             convert = "onlycorrect")
    } else {
      cropsuit <- readSource("Zabel2014", subtype = paste("all_marginal",
                                                          unlist(strsplit(marginal_land, split = ":"))[2],
                                                          sep = ":"),
                             convert = "onlycorrect")
    }

    # make sure that suitable cropland is not larger than total land area
    cropsuit <- pmin(cropsuit, landarea)
    # set upper bound for cropland at grid cell level
    # in each grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, luh[, , "crop"])

    tmp <- cropsuit
    getNames(tmp) <- "all_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("q33_marginal", marginal_land)) || marginal_land == "magpie") {


    if (marginal_land == "magpie") {
      cropsuit <- readSource("Zabel2014", subtype = paste("q33_marginal", "rainfed_and_irrigated",
                                                          sep = ":"),
                             convert = "onlycorrect")
    } else {
      cropsuit <- readSource("Zabel2014", subtype = paste("q33_marginal",
                                                          unlist(strsplit(marginal_land, split = ":"))[2],
                                                          sep = ":"),
                             convert = "onlycorrect")
    }
    # make sure that suitable cropland is not larger than total land area
    cropsuit <- pmin(cropsuit, landarea)
    # set upper bound for cropland at grid cell level
    # in each grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, luh[, , "crop"])

    tmp <- cropsuit
    getNames(tmp) <- "q33_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("q50_marginal", marginal_land))) {

    cropsuit <- readSource("Zabel2014", subtype = paste("q50_marginal",
                                                        unlist(strsplit(marginal_land, split = ":"))[2],
                                                        sep = ":"),
                           convert = "onlycorrect")
    # make sure that suitable cropland is not larger than total land area
    cropsuit <- pmin(cropsuit, landarea)
    # set upper bound for cropland at grid cell level
    # in each grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, luh[, , "crop"])

    tmp <- cropsuit
    getNames(tmp) <- "q50_marginal"
    x <- mbind(x, tmp)

  }


  if (any(grepl("q66_marginal", marginal_land))) {

    cropsuit <- readSource("Zabel2014", subtype = paste("q66_marginal",
                                                        unlist(strsplit(marginal_land, split = ":"))[2],
                                                        sep = ":"),
                           convert = "onlycorrect")

    # make sure that suitable cropland is not larger than total land area
    cropsuit <- pmin(cropsuit, landarea)
    # set upper bound for cropland at grid cell level
    # in each grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, luh[, , "crop"])

    tmp <- cropsuit
    getNames(tmp) <- "q66_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("q75_marginal", marginal_land))) {

    cropsuit <- readSource("Zabel2014", subtype = paste("q75_marginal",
                                                        unlist(strsplit(marginal_land, split = ":"))[2],
                                                        sep = ":"),
                           convert = "onlycorrect")

    # make sure that suitable cropland is not larger than total land area
    cropsuit <- pmin(cropsuit, landarea)
    # set upper bound for cropland at grid cell level
    # in each grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, luh[, , "crop"])

    tmp <- cropsuit
    getNames(tmp) <- "q75_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("no_marginal", marginal_land)) || marginal_land == "magpie") {

    if (marginal_land == "magpie") {
      cropsuit <- readSource("Zabel2014", subtype = paste("no_marginal", "rainfed_and_irrigated",
                                                          sep = ":"),
                             convert = "onlycorrect")
    } else {
      cropsuit <- readSource("Zabel2014", subtype = paste("no_marginal",
                                                          unlist(strsplit(marginal_land, split = ":"))[2],
                                                          sep = ":"),
                             convert = "onlycorrect")
    }
    # make sure that suitable cropland is not larger than total land area
    cropsuit <- pmin(cropsuit, landarea)
    # set upper bound for cropland at grid cell level
    # in each grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, luh[, , "crop"])

    tmp <- cropsuit
    getNames(tmp) <- "no_marginal"
    x <- mbind(x, tmp)

  }

  if (country_level) {

    out <- toolAggregateCell2Country(collapseDim(x, dim = "iso"), fill = 0)

  } else {

    if (cells == "magpiecell") {

      out <- toolCoord2Isocell(x)

    } else if (cells == "lpjcell") {

      out <- x

    } else {
      stop("Please specify cells argument")
    }
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Cropland suitability based on Zabel et al. (2014)
              with different suitability thresholds
              ('all_marginal', 'q33_marginal', 'no_marginal').",
              isocountries = FALSE))
}
