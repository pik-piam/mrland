#' @title readZabel2014
#' @description Reads crop suitability data published in Zabel, F., Putzenlechner, B., & Mauser, W. (2014).
#' Global Agricultural Land Resources – A High Resolution Suitability Evaluation and Its Perspectives until
#' 2100 under Climate Change Conditions. PLOS ONE, 9(9), e107522. https://doi.org/10.1371/journal.pone.0107522
#' and extracts the share of suitable cropland per grid cell, depending on different suitability thresholds.
#' @param subtype Define which marginal land should be included (see options below) and 
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
#' }
#' @return Returns magpie objects with the share of suitable cropland per grid cell
#' @author Patrick v. Jeetze, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("Zabel2014", subtype = "all_marginal", convert = "onlycorrect")
#' }
#'
#' @importFrom terra rast classify extract aggregate terraOptions
#' @importFrom withr local_tempdir defer
#' @importFrom mrcommons toolGetMappingCoord2Country
#'

readZabel2014 <- function(subtype = "all_marginal:rainfed_and_irrigated") {

  # extract subtype
  condition <- unlist(strsplit(subtype, split = ":"))[2]
  subtype   <- unlist(strsplit(subtype, split = ":"))[1]

  # set terra options and temporary directory
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terraOptions(tempdir = tempdir()))

  # read data
  if (condition == "rainfed_and_irrigated") {

      cropsuitZabel <- rast(paste0("./cropsuitability_rainfed_and_irrigated/1981-2010/",
                                   "overall_cropsuit_i_1981-2010/overall_cropsuit_i_1981-2010.tif"))

  } else if (condition == "rainfed") {

      cropsuitZabel <- rast(paste0("./cropsuitability_rainfed_only/1981-2010/",
                                   "overall_suitability_1/overall_suitability_rainfed_1981-2010.tif"))

  } else if (condition == "irrigated") {

      cropsuitZabel <- rast(paste0("./cropsuitability_irrigated_only/1981-2010/",
                                  "overall_suitability_1/overall_suitability_irrigated_1981-2010.tif"))

  } else {
    stop("Please select which suitability condition should be selected: 
    rainfed, irrigated, or rainfed_and_irrigated whereas the latter includes
    potentially rainfed and acutally irrigated areas")
  }

  # define suitability threshold for crop suitability in MAgPIE at original resolution of 30 arc seconds
  # In Zabel et al. (2014) marginal land is defined by a suitability index <= 0.33

  if (subtype == "all_marginal") {

    # all marginal land is included
    rclassMatrx <- matrix(c(
      0, 0, NA,
      0.10, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    cropsuitZabel <- classify(cropsuitZabel, rclassMatrx, include.lowest = TRUE)
  } else if (subtype == "q33_marginal") {

    # The bottom tertile (suitability index below 0.13) of the marginal land area is excluded
    rclassMatrx <- matrix(c(
      0, 0.13, NA,
      0.13, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    cropsuitZabel <- classify(cropsuitZabel, rclassMatrx, include.lowest = TRUE)
  } else if (subtype == "q50_marginal") {

    # The bottom  half (suitability index below 0.18) of the marginal land area is excluded
    rclassMatrx <- matrix(c(
      0, 0.18, NA,
      0.18, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    cropsuitZabel <- classify(cropsuitZabel, rclassMatrx, include.lowest = TRUE)
  } else if (subtype == "q66_marginal") {

    # The first and second tertile (suitability index below 0.23) of the marginal land area are excluded
    rclassMatrx <- matrix(c(
      0, 0.23, NA,
      0.23, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    cropsuitZabel <- classify(cropsuitZabel, rclassMatrx, include.lowest = TRUE)
  } else if (subtype == "q75_marginal") {

    # The first, second and third quartiles (suitability index below 0.25) of the marginal land are are excluded
    rclassMatrx <- matrix(c(
      0, 0.25, NA,
      0.25, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    cropsuitZabel <- classify(cropsuitZabel, rclassMatrx, include.lowest = TRUE)
  } else if (subtype == "no_marginal") {

    # marginal land (suitability index below 0.33) is fully excluded
    rclassMatrx <- matrix(c(
      0, 0.33, NA,
      0.33, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    cropsuitZabel <- classify(cropsuitZabel, rclassMatrx, include.lowest = TRUE)
  }

  # aggregate and sum up area (Mha) of suitable pixels (1) per 0.5 degree grid cell
  # aggregation factor from 30 arc sec to 0.5 degree: 60
  cropsuitZabelArea <- cellSize(cropsuitZabel, unit = "ha") * 1e-6
  cropsuitZabel05 <- aggregate(cropsuitZabelArea, fact = 60, fun = sum, na.rm = TRUE)

  ### Create magpie object

  # get spatial mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  out <- as.magpie(extract(cropsuitZabel05, map[c("lon", "lat")])[, 2], spatial = 1)
  # set dimension names
  dimnames(out) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."), "t" = NULL, "data" = subtype)

  return(out)
}
