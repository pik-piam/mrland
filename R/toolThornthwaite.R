#' @title toolThornthwaite
#' @description This tool to calculate potential evapotranspiration (PET) based on the
#'              Thornthwaite function (see SPEI::thornthwaite for base of this re-implementation
#'              https://rdrr.io/cran/SPEI/src/R/thornthwaite.R)
#'
#' @param temp     monthly mean temperature
#' @param lat      lattitude of the cell
#'
#' @return PET in magclass format
#' @author Kristine Karstens
#'
#' @importFrom magclass dimSums new.magpie getItems getSets
#'
#' @export

toolThornthwaite <- function(temp, lat) {

  if (!is.magpie(temp)) {
    stop("'temp' is not a MAgPIE object!")
  }

  # Remove negative temperatures from time series
  temp[temp < 0] <- 0

  # Create magpie objects for lattitudes an month helper variables
  magLat   <- new.magpie(cells_and_regions = getItems(temp, 1),
                         fill = lat, sets = getSets(temp))
  mlen     <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  msum     <- cumsum(mlen) - mlen + 15
  magMlen  <- new.magpie(names = getItems(temp, 3), fill = mlen)
  magMsum  <- new.magpie(names = getItems(temp, 3), fill = msum)

  ### Computation of PET - - - - - - - - - - - - - - - - - - - - - - - - -

  # Monthly correction factor, depending on latitude (K)
  tanLat <- tan(magLat / 57.2957795)
  # mean solar declination angle for each month (Delta)
  delta <- 0.4093 * sin(((2 * pi * magMsum) / 365) - 1.405)
  # hourly angle of sun rising (omega)
  tanDelta    <- tan(delta)
  tanLatDelta <- tanLat * tanDelta
  tanLatDelta <- ifelse(tanLatDelta < (-1), -1, tanLatDelta)
  tanLatDelta <- ifelse(tanLatDelta > 1, 1, tanLatDelta)
  omega       <- acos(-tanLatDelta)
  # mean daily daylight hours for each month (N)
  n <- 24 / pi * omega
  # which leads to K
  k <- n / 12 * magMlen / 30
  # Annual temperature efficiency index (J)
  j <- dimSums((temp / 5)^1.514, dim = 3)
  # Empirical exponent (q)
  a <- 0.000000675 * j^3 - 0.0000771 * j^2 + 0.01792 * j + 0.49239
  # Potential evapotranspiration series (PE)
  pet <-  (10 * temp / j) ^ a * k * 16
  # replace all zero devisions with 0
  pet[is.nan(pet)] <- 0

  # check for nans and more
  if (any(!is.finite(pet))) warning("Data containing inconsistencies.")

  return(pet)
}
