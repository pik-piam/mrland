% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCopernicus.R
\name{readCopernicus}
\alias{readCopernicus}
\title{readCopernicus}
\usage{
readCopernicus(subtype = "CroplandTreecover")
}
\arguments{
\item{subtype}{For cropland area covered by trees choose \code{"CroplandTreecover"}.
For cropland area requiring relocation in response to increasing SNV choose \code{"SNVTargetCropland"}.}
}
\value{
Returns magpie objects with cropland area covered by trees or cropland area
requiring relocation in order to increase SNV in farmed landscapes.
}
\description{
Reads either information on the area on cropland covered by trees or
information the cropland area that requires relocation in response of increasing
semi-natural vegetation in farmed landscapes. The data was derived from high resolution
land cover information (LC100) from the Copernicus Global Land Service.
(https://zenodo.org/records/3939050)
}
\examples{
\dontrun{
readSource("Copernicus", subtype = "CroplandTreecover", convert = "onlycorrect")
}

}
\author{
Patrick v. Jeetze
}
