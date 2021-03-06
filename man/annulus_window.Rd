% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annulus_window.R
\name{annulus_window}
\alias{annulus_window}
\title{Creates annulus focal window}
\usage{
annulus_window(inner_rad, outer_rad, type = 1)
}
\arguments{
\item{inner_rad}{radius of inner annulus in cell units (must be at least 1 and less than or equal to outer radius)}

\item{outer_rad}{radius of inner annulus in cell units}

\item{type}{Either 1 or 2. If type = "1" then weights will be 0 or 1. if type ="2", weights will sum to 1. Default is 1.}
}
\description{
Creates annulus focal window around central pixel.
https://desktop.arcgis.com/en/arcmap/10.3/analyze/arcpy-spatial-analyst/nbrannulus-class.htm
}
