% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inputs_SWAT.R
\name{grid_tas2SWAT}
\alias{grid_tas2SWAT}
\title{Function to extract Temperature grid data from daily and make inputs for SWAT model}
\usage{
grid_tas2SWAT(
  tx = NULL,
  tn = NULL,
  Subbasins = NULL,
  DEM = NULL,
  cores = parallel::detectCores() - 1,
  Star_Date = "1981-01-01",
  Temporal = "day",
  dsn = getwd()
)
}
\arguments{
\item{tx}{brick or stack raster/netcdf grid format of maximun temperature.}

\item{tn}{brick or stack raster/netcdf grid format of minimun temperature.}

\item{Subbasins}{SpatialPolygonDataFrame object of subbasins generated by SWAT model.}

\item{DEM}{Raster .tif format of Digital Elevation Model which was used in the hydrological model.}

\item{cores}{numeric. Number of cores to be used for the download. By default,
it automatically detects how many cores there are on your computer and
subtracts 1 to prevent it from slowing down.}

\item{Star_Date}{character. Initial date to extract the information.}

\item{Temporal}{character. Only \code{'day'} is aceptting for swat, but if you want to extract monthly data, write as \code{'month'}}

\item{dsn}{path. Directory to which you want to store the downloaded data.}
}
\description{
Function to extract Temperature grid data from daily and make inputs for SWAT model
}
\examples{
\dontrun{
DEM <- raster::raster('Data/GRIDs/DEM.tif')
tx <- 'Data/GRIDs/tmax_diaria.nc'
tn <- 'Data/GRIDs/tmin_diaria.nc'
Subbasins <- rgdal::readOGR('Data/SHPs/subs.shp')
Star_Date = '1981-01-01'
Temporal = 'day'
grid_tas2SWAT (
 tx = tx,
 tn = tn,
 Subbasins = Subbasins,
 DEM = DEM,
 Star_Date = '1981-01-01',
 Temporal = 'day')


}
}
\seealso{
Other Inputs Hidrological models: 
\code{\link{grid2RSMinerve}()},
\code{\link{grid_pcp2SWAT}()},
\code{\link{stations2RSMinerve}()}
}
\author{
Crhistian Cornejo
}
\concept{Inputs Hidrological models}
\concept{Inputs SWAT}
