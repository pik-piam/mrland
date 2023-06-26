#' @title readGLW4
#' @description reads in Gridded Livestock of the World v4,
#' downloaded from: https://dataverse.harvard.edu/dataverse/glw_4
#' @return A gridded magpie object with gridded livstock of the world
#' @param subtype Weighting method and livestock type:
#'        \itemize{
#'        \item {Da}: Dasymetric weighting informed by Random Forest
#'        \item {Aw}: Areal weighting (distributed uniformly in each census)
#'        \itemize{
#'        \item \code{Ch}: Chicken
#'        \item \code{Ct}: Cattle
#'        \item \code{Pg}: Pigs
#'        \item \code{Sh}: Sheep
#'        \item \code{Gt}: Goats
#'        \item \code{Ho}: Horse
#'        \item \code{Dk}: ducks
#'        \item \code{Bf}: Buffaloes
#'        }}
#'
#' @author David M Chen
#' @importFrom terra aggregate

readGLW4 <- function(subtype = "Da_Ct") {

  file <- toolSubtypeSelect(subtype, c(Da_Ch = "Ch/5_Ch_2015_Da.tif",
                                          Da_Ct = "Ct/5_Ct_2015_Da.tif",
                                          Da_Pg = "Pg/5_Pg_2015_Da.tif",
                                          Da_Sh = "Sh/5_Sh_2015_Da.tif",
                                          Da_Gt = "Gt/5_Gt_2015_Da.tif",
                                          Da_Ho = "Ho/5_Ho_2015_Da.tif",
                                          Da_Dk = "Dk/5_Dk_2015_Da.tif",
                                          Da_Bf = "Bf/5_Bf_2015_Da.tif",
                                          Aw_Ch = "Ch/6_Ch_2015_Aw.tif",
                                          Aw_Ct = "Ct/6_Ct_2015_Aw.tif",
                                          Aw_Pg = "Pg/6_Pg_2015_Aw.tif",
                                          Aw_Sh = "Sh/6_Sh_2015_Aw.tif",
                                          Aw_Gt = "Gt/6_Gt_2015_Aw.tif",
                                          Aw_Ho = "Ho/6_Ho_2015_Aw.tif",
                                          Aw_Dk = "Dk/6_Dk_2015_Aw.tif",
                                          Aw_Bf = "Bf/6_Bf_2015_Aw.tif"))

  x <- rast(file)
  x <- aggregate(x, fact = 6, fun = sum)
  x <- as.magpie(x)

  return(x)

}
