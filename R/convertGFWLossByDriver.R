#' @title convertGFWLossByDriver
#'
#' @description Moves the GFW data from the GADM country set to madrat's ISO country set: Kosovo,
#' Northern Cyprus and the Akrotiri and Dhekelia base areas are added to Serbia and Cyprus, GADM's
#' placeholder codes for disputed territories are dropped and countries GFW does not list are
#' filled with zero.
#'
#' @param x magpie object as returned by \code{\link{readGFWLossByDriver}}
#' @param subtype \code{"loss"} or \code{"extent"}, as passed to the read function; the conversion
#' is the same for both
#' @return magpie object on madrat's ISO country set, Mha
#' @author Michael Crawford
#' @importFrom madrat toolCountryFill
#' @importFrom magclass getItems
#' @seealso \code{\link{readGFWLossByDriver}}
#' @examples
#' \dontrun{
#' a <- readSource("GFWLossByDriver", convert = TRUE)
#' }

convertGFWLossByDriver <- function(x, subtype = "loss") {

  # GADM codes inside a madrat country; their loss is added to the host rather than dropped
  gfwIsoHosts <- c(XKO = "SRB",  # Kosovo
                   ZNC = "CYP",  # Northern Cyprus
                   XAD = "CYP")  # Akrotiri and Dhekelia
  # GADM placeholders for disputed territories, together about 0.03 per cent of global loss
  gfwIsoDropped <- sprintf("Z%02d", 1:9)

  before <- sum(x)

  for (code in names(gfwIsoHosts)) {
    host <- gfwIsoHosts[[code]]
    if (code %in% getItems(x, 1)) {
      if (host %in% getItems(x, 1)) {
        x[host, , ] <- x[host, , ] + x[code, , ]
      } else {
        getItems(x, 1)[getItems(x, 1) == code] <- host
        next
      }
      x <- x[setdiff(getItems(x, 1), code), , ]
    }
  }

  out <- toolCountryFill(x, fill = 0, no_remove_warning = gfwIsoDropped)

  # a new GADM code carrying real loss must fail here rather than vanish
  lost <- (before - sum(out)) / before
  if (lost > 0.005) {
    stop("GFWLossByDriver: reconciling GADM with madrat's ISO set dropped ",
         round(100 * lost, 3), " per cent of global loss, above the 0.5 per cent tolerance. ",
         "Codes not in madrat: ",
         toString(setdiff(getItems(x, 1), madrat::getISOlist())), ".")
  }

  return(out)
}
