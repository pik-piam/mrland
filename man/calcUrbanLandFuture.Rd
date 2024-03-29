% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcUrbanLandFuture.R
\name{calcUrbanLandFuture}
\alias{calcUrbanLandFuture}
\title{calcUrbanLandFuture}
\usage{
calcUrbanLandFuture(
  timestep = "5year",
  subtype = "LUH2v2",
  cells = "lpjcell",
  cellular = TRUE
)
}
\arguments{
\item{timestep}{5year or yearly}

\item{subtype}{where the data source comes from ("LUH2v2" or "Gao")}

\item{cells}{magpiecell (59199 cells) or lpjcell (67420 cells)}

\item{cellular}{TRUE for results on 0.5 degree grid.}
}
\value{
List of magpie objects with results on 0.5deg grid level, weights NULL, unit and description.
}
\description{
Urban land in Mha on 0.5deg grid
}
\author{
David Chen, Patrick v. Jeetze, Felicitas Beier
}
