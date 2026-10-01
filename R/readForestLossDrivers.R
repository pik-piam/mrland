#' @title readForestLossDrivers
#'
#' @description Reads Table 1 of Curtis et al. (2018), tree cover loss 2001-2015 by world region
#' and dominant driver, transcribed by hand from the article into \code{forest_loss_fixed.csv}.
#'
#' @return magpie object with the mean annual loss per Curtis region and driver, Mha
#' @author Abhijeet Mishra, Michael Crawford
#' @references Curtis, P. G., Slay, C. M., Harris, N. L., Tyukavina, A. and Hansen, M. C. (2018)
#' Classifying drivers of global forest loss. Science 361, 1108-1111. doi:10.1126/science.aau3445
#' @seealso \code{\link{calcForestLossByDriver}}
#' @examples
#' \dontrun{
#' a <- readSource("ForestLossDrivers", convert = FALSE)
#' }

readForestLossDrivers <- function() {
  # loss 2001-2015 in Mha and driver shares in per cent. Rows are normalised to sum to 100: the
  # printed rows sum to 99-102 (rounding, and cells printed as "<1%"), while the drivers must sum
  # to the observed loss rather than exceed it.
  df <- read.csv("forest_loss_fixed.csv")
  drivers <- c("deforestation", "shifting_agriculture", "forestry", "wildfire", "urbanization")
  df[, drivers] <- df$treecoverloss_01_15 * df[, drivers] / 100 / 15
  return(as.magpie(df[, c("region", drivers)], temporal = NULL, spatial = "region"))
}
