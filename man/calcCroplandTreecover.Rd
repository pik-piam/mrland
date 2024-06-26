% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcCroplandTreecover.R
\name{calcCroplandTreecover}
\alias{calcCroplandTreecover}
\title{calcCroplandTreecover}
\usage{
calcCroplandTreecover(
  maginput = TRUE,
  cells = "magpiecell",
  countryLevel = FALSE
)
}
\arguments{
\item{maginput}{Whether data should be corrected to align with cropland
initialised in MAgPIE.}

\item{cells}{magpiecell (59199 cells) or lpjcell (67420 cells)}

\item{countryLevel}{Whether output shall be at country level.
Requires aggregate=FALSE in calcOutput.}
}
\value{
List with a magpie object
}
\description{
Returns area on cropland covered by trees (Mha).
}
\examples{
\dontrun{
calcOutput("CroplandTreecover", aggregate = FALSE)
}

}
\seealso{
\code{\link{readCopernicus}}
}
\author{
Patrick v. Jeetze
}
