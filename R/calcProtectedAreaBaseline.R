#' @title calcProtectedAreaBaseline
#'
#' @description Returns protected land area (Mha) in terms of cropland, pasture, forest and other land between 1995 and 2020.
#'
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return List with a magpie object
#' @author Patrick v. Jeetze
#' @seealso
#' \code{\link{calcProtectArea}}
#'
#' @examples
#' \dontrun{
#' calcOutput("ProtectedAreaBaseline", aggregate = FALSE)
#' }
#'
#' @importFrom mrcommons toolCoord2Isocell
#'
calcProtectedAreaBaseline <- function(cells = "magpiecell") {
  x <- readSource("ProtectedAreaBaseline")

  if (cells == "magpiecell") {
    out <- toolCoord2Isocell(x)
  } else if (cells == "lpjcell") {
    out <- x
  } else {
    stop("Please specify cells argument")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = "Protected land area in terms of cropland, pasture, forest and other land between 1995 and 2020.",
    isocountries = FALSE
  ))
}
