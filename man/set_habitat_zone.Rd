% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_habitat_zone.R
\name{set_habitat_zone}
\alias{set_habitat_zone}
\title{Set Habitat Zone}
\usage{
set_habitat_zone(tidal_zone, salinity_zone)
}
\arguments{
\item{tidal_zone}{raster; A tidal zone raster.}

\item{salinity_zone}{raster; A salinity zone raster.}
}
\value{
A categorical habitat zone RasterLayer where:
\enumerate{
\item Upland
\item Marine-Deep
\item Marine-Subtidal
\item Marine-Intertidal
\item Estuarine-Subtidal
\item Estuarine-Intertidal
\item Freshwater
}
}
\description{
Creates a habitat zone raster from the input tidal zone and
salinity zones rasters.
}
