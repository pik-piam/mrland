#' @title readGLW4
#' @description reads in Gridded Livestock of the World v4,
#' downloaded from: https://dataverse.harvard.edu/dataverse/glw_4 (2015)
#' and https://data.apps.fao.org/catalog/iso/9d1e149b-d63f-4213-978b-317a8eb42d02 (2020)
#' @return A gridded magpie object with gridded livestock counts.
#'   2015 data: heads per 0.5-degree pixel (aggregated by sum).
#'   2020 data: heads per km2 (aggregated by mean, native density unit).
#' @param subtype Weighting method, livestock species, and reference year
#'   (\code{"<method>_<species>_<year>"}):
#'        \itemize{
#'        \item {Da}: Dasymetric weighting informed by Random Forest
#'        \item {Aw}: Areal weighting – 2015 only
#'        \itemize{
#'        \item \code{Ch}: Chicken
#'        \item \code{Ct}: Cattle
#'        \item \code{Pg}: Pigs
#'        \item \code{Sh}: Sheep
#'        \item \code{Gt}: Goats
#'        \item \code{Ho}: Horse (2015 only)
#'        \item \code{Dk}: Ducks (2015 only)
#'        \item \code{Bf}: Buffaloes
#'        }}
#'
#' @author David M Chen, Bin Lin
#' @examples
#' \dontrun{
#' readSource("GLW4", subtype = "Da_Ct_2015", convert = FALSE)
#' readSource("GLW4", subtype = "Da_Ct_2020", convert = FALSE)
#' }
#' @importFrom terra rast aggregate
#' @importFrom madrat toolSubtypeSelect
#' @importFrom magclass as.magpie getYears<- getNames<-

readGLW4 <- function(subtype = "Da_Ct_2015") {

  subtypes2015 <- c(
    Da_Ch_2015 = "5_Ch_2015_Da.tif",
    Da_Ct_2015 = "5_Ct_2015_Da.tif",
    Da_Pg_2015 = "5_Pg_2015_Da.tif",
    Da_Sh_2015 = "5_Sh_2015_Da.tif",
    Da_Gt_2015 = "5_Gt_2015_Da.tif",
    Da_Ho_2015 = "5_Ho_2015_Da.tif",
    Da_Dk_2015 = "5_Dk_2015_Da.tif",
    Da_Bf_2015 = "5_Bf_2015_Da.tif",
    Aw_Ch_2015 = "6_Ch_2015_Aw.tif",
    Aw_Ct_2015 = "6_Ct_2015_Aw.tif",
    Aw_Pg_2015 = "6_Pg_2015_Aw.tif",
    Aw_Sh_2015 = "6_Sh_2015_Aw.tif",
    Aw_Gt_2015 = "6_Gt_2015_Aw.tif",
    Aw_Ho_2015 = "6_Ho_2015_Aw.tif",
    Aw_Dk_2015 = "6_Dk_2015_Aw.tif",
    Aw_Bf_2015 = "6_Bf_2015_Aw.tif"
  )

  subtypes2020 <- c(
    Da_Ct_2020 = "GLW4-2020.D-DA.CTL.tif",
    Da_Sh_2020 = "GLW4-2020.D-DA.SHP.tif",
    Da_Pg_2020 = "GLW4-2020.D-DA.PGS.tif",
    Da_Bf_2020 = "GLW4-2020.D-DA.BFL.tif",
    Da_Ch_2020 = "GLW4-2020.D-DA.CHK.tif",
    Da_Gt_2020 = "GLW4-2020.D-DA.GTS.tif"
  )

  if (subtype %in% names(subtypes2015)) {
    # 2015 data: units are heads/pixel → aggregate by sum
    file <- toolSubtypeSelect(subtype, subtypes2015)
    x <- rast(file)
    x <- aggregate(x, fact = 6, fun = sum, na.rm = TRUE)
  } else {
    # 2020 data: units are heads/km² → aggregate by mean to preserve density units
    file <- toolSubtypeSelect(subtype, subtypes2020)
    x <- rast(file)
    x <- aggregate(x, fact = 6, fun = mean, na.rm = TRUE)
  }

  x <- as.magpie(x)
  x[is.nan(x)] <- NA
  getYears(x) <- paste0("y", substr(subtype, nchar(subtype) - 3, nchar(subtype)))
  getNames(x) <- subtype
  attr(x, "unit") <- if (subtype %in% names(subtypes2015)) "heads/pixel" else "heads/km2"
  return(x)

}
