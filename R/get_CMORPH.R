#### FUNCTION TO GET CMORPH GRID DATA WITH SUBSETTING IN A ROI EXTENSION
################################################################################
#' @title  Function to subset CMORPH raster data to a roi extension.
#' @param path_files list. Path to the files where the CMORPH.nc are downloaded.
#' @param file_out path. Path where the new files will be saved. By default they are saved in the same path as
#' the downloaded files and are automatically renamed.
#' @param vars list. Variables necessary to perform the subsetting, do not change it for CMORPH grids.
#' @param cores numeric. Number of cores to be used for the download. By default,
#' it automatically detects how many cores there are on your computer and
#' subtracts 1 to prevent it from slowing down.
#' @param roi numeric. Vector with xmax, xmin, ymax, ymin values for a region for subsetting the data.1
#' @return subsetting tif.
#' @family subsetting functions
#'
#' @author Crhistian Cornejo
#'
#' @references
#' Joyce, R. J., J. E. Janowiak, P. A. Arkin, and P. Xie, 2004:
#' CMORPH: A method that produces global precipitation estimates from
#' passive microwave and infrared data at high spatial and temporal resolution.. J. Hydromet., 5, 487-503.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#' roi = c(-86,-66,-20,2)
#' files <- base::list.files(path = paste0(dsn,'/download_CMORPH/daily/0.25deg/'),
#' pattern = "*.nc$",all.files=TRUE, full.names= T)
#' subsetting_CMORPH.nc <- function(
#'    path_files = files,
#'    roi = c(-86,-66,-20,2),
#'    vars = list (Lat = 'lat',
#'                 Long = 'lon',
#'                 main_var = 'cmorph',
#'                 time = 'time'),
#'    file_out = sub("[.]nc", "_subsetting.nc", path_files),
#'    cores = 4)
#'  }
#' }
#' @rdname subsetting_CMORPH
#' @importFrom parallel detectCores makeCluster
#' @importFrom raster stack
#' @importFrom doParallel registerDoParallel
#' @importFrom tictoc tic toc
#' @importFrom foreach foreach
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get ncdim_def ncvar_def nc_create ncvar_put nc_close
#'
subsetting_CMORPH <- function(
    path_files = getwd(),
    roi = c(-86,-66,-20,2),
    vars = list (Lat = 'lat',
                 Long = 'lon',
                 main_var = 'cmorph',
                 time = 'time'),
    file_out = sub("[.]nc", "_subsetting.nc", path_files),
    cores = parallel::detectCores()-1){

  stack <- raster::stack(path_files)

  # Reading data
  xmin    <- roi[1]
  xmax    <- roi[2]
  ymin    <- roi[3]
  ymax    <- roi[4]

  # Creating subsseting CMORPH GRID

  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  tictoc::tic()
  foreach::foreach (i=1:length(path_files)) %dopar% {

    nc   <- ncdf4::nc_open(path_files[i])

    # Longitude dimension ------------------------------------------------
    if(xmax<0 & xmin<0){
      lon    <- ncdf4::ncvar_get(nc, as.character(vars[2]))-360
      lonunits <- "degrees_west"
    }else{
      lon  <- ncdf4::ncvar_get(nc, as.character(vars[2]))
      lonunits <- "degrees_east"
    }

    # Latitude dimension --------------------------------------------------
    if(ymin<0){
      lat      <- ncdf4::ncvar_get(nc, as.character(vars[1]))
      latunits <- "degrees_south"
    }else{
      lat      <- ncdf4::ncvar_get(nc, as.character(vars[1]))
      latunits <- "degrees_north"
    }

    # Time dimension ------------------------------------------------------
    time   <- ncdf4::ncvar_get(nc, as.character(vars[4]))
    tunits <- ncdf4::ncatt_get(nc,as.character(vars[4]),'units')$value
    ntime  <- length(time)
    calendar <- ncdf4::ncatt_get(nc,as.character(vars[4]),'calendar')$value

    # Main Variable dimension -----------------------------------------------
    dat      <- ncdf4::ncvar_get(nc, as.character(vars[3]))
    units    <- ncdf4::ncatt_get(nc,as.character(vars[3]),'units')$value
    longname <- ncdf4::ncatt_get(nc,as.character(vars[3]),"long_name")$value
    missval  <- ncdf4::ncatt_get(nc,as.character(vars[3]),"missing_value")$value

    # Subsetting grid --------------------------------------------------------
    lon_sub <- subset(lon, lon>=xmin & lon<=xmax)
    lat_sub <- rev(subset(lat, lat>=ymin & lat<=ymax))
    dat_sub <- dat[match(lon_sub, lon),match(lat_sub, lat)]

    # Create a new netcdf file
    londim    <- ncdf4::ncdim_def(name="lon",
                                  units=lonunits,
                                  vals=as.double(lon_sub))
    latdim    <- ncdf4::ncdim_def(name="lat",
                                  units=latunits,
                                  vals=as.double(lat_sub))
    timedim   <- ncdf4::ncdim_def(name="time",
                                  units=tunits,
                                  vals=as.double(time),
                                  calendar=calendar)
    vardef    <- ncdf4::ncvar_def(name='cmorph',
                                  units=units,
                                  dim=list(londim,latdim,timedim),
                                  missval=missval,
                                  longname=longname,
                                  prec="float")

    filename <- file_out[i]
    ncnew    <- ncdf4::nc_create(filename=filename,
                                 vars=list(vardef),
                                 force_v4=TRUE)
    ncdf4::ncvar_put(nc=ncnew,
                     varid=vardef,
                     vals=dat_sub)
    ncdf4::nc_close(nc)
    ncdf4::nc_close(ncnew)
    gc(reset=TRUE)
  }
  tictoc::toc()
  cat('\f')
  cat('-------- Finish subsetting!! -----------\n')
}


################################################################################
#### FUNCTION TO GET CMORPH GRID DATA WITH SUBSETTING IN A ROI EXTENSION
################################################################################
#' @title Function to download CMORPH raster data global on daily sacale at 25km.
#' @param star_date numeric. Start Date(s) for which you want to download CMORPH data.
#' @param end_date numeric. End Date(s) for which you want to download CMORPH data. recommended to download at least one year.
#' @param dsn path. Directory to which you want to store the downloaded data.
#' @param cores numeric. Number of cores to be used for the download. By default,
#' it automatically detects how many cores there are on your computer and
#' subtracts 1 to prevent it from slowing down.
#' @param verbose logical. If \code{TRUE} this will show the task process for the parallel process.
#' @param subsetting logical. If \code{TRUE} all downloaded netcdf's will be subsetting to the extension set to \code{roi}.
#' @param roi numeric. Vector with xmax, xmin, ymax, ymin values for a region for subsetting the data.
#'
#' @importFrom parallel detectCores makeCluster
#' @importFrom glue glue
#' @importFrom xfun dir_create
#' @importFrom lubridate year
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach getDoParWorkers foreach
#' @importFrom tictoc tic toc tic.log tic.clearlog
#' @importFrom utils download.file
#'
#' @family Download grid data
#'
#' @author Crhistian Cornejo
#'
#' @references
#' Joyce, R. J., J. E. Janowiak, P. A. Arkin, and P. Xie, 2004:
#' CMORPH: A method that produces global precipitation estimates from
#' passive microwave and infrared data at high spatial and temporal resolution.. J. Hydromet., 5, 487-503.
#'
#' @examples
#' \dontrun{
#' roi = c(-86,-66,-20,2)
#' download_CMORPH(star_date = '1998-01-01',
#'                 end_date = '1998-12-31',
#'                 roi = roi,
#'                 subsetting = TRUE,
#'                 verbose = FALSE)
#' }
#'
#' @export

download_CMORPH <- function(

  star_date = '1998-01-01',
  end_date = '1998-03-01',
  dsn   = getwd(),
  cores = parallel::detectCores()-1,
  verbose = FALSE,
  subsetting = TRUE,
  roi= c(-86,-66,-20,2)){

  #error messages! ------------------------------------------------------------
  if (is.null(roi) & subsetting == TRUE) {

    stop("You need a roi extension to make the subsetting!")

  }

  options(warn = - 1)

  ## Requirements!--------------------------------------------------------------

  require(doParallel)
  require(foreach)
  require(dplyr)

  base_url <- 'https://www.ncei.noaa.gov/data/cmorph-high-resolution-global-precipitation-estimates/access/' #fron NOAA
  tres <- 'daily'
  sres <- '0.25deg'
  dates <- seq.Date(as.Date(star_date), as.Date(end_date),'days')
  outp <- glue::glue(dsn,'/download_CMORPH/{tres}/{sres}/')
  ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

  #get year/day
  months <- seq.Date(as.Date(star_date), as.Date(end_date),'months')
  months <- base::substring(months,1,7)
  months <- base::gsub('-','/', as.character(months))

  #vector of months per day
  months_30days <- c('04','06','09','11')
  months_31days <- c('01','03','05','07','08','10','12')
  months_28days <- '02'

  just_months <- substring(months,6,7)
  dates_dw <- base::gsub('-', '', as.character(dates))
  years <- lubridate::year(dates)
  bic_years <- c(2000, 2004, 2008, 2012, 2016, 2020, 2024)

  #MAKING URLS FOR DIFFERENT MONTHS --------------------------------------------------------------------------------
  #for months with 31days ----------------------------
  years_months31 <- paste0(years,'/',months_31days)
  months31 <- base::sort(rep(months,31))
  months31 <- base::gsub('-', '/', as.character(months31))
  months31 <- months31[months31 %in% years_months31]
  days31 <- paste0(months31,'/',sprintf('%00.2d', 1:31))
  days31 <- base::gsub('/','',as.character(days31))
  days31 <- days31[days31 %in% dates_dw]

  url_31days <- sort(paste0(base_url,tres,'/',sres,'/',months31,'/','CMORPH_V1.0_ADJ_0.25deg-DLY_00Z_',days31,'.nc'))

  #for months with 30days ----------------------------
  years_months30 <- paste0(years,'/',months_30days)
  months30 <- base::sort(rep(months,30))
  months30 <- gsub('-', '/', as.character(months30))
  months30 <- months30[months30 %in% years_months30]
  days30 <- paste0(months30,'/',sprintf('%00.2d', 1:30))
  days30 <- base::gsub('/','',as.character(days30))
  days30 <- days30[days30 %in% dates_dw]

  url_30days <- sort(paste0(base_url,tres,'/',sres,'/',months30,'/','CMORPH_V1.0_ADJ_0.25deg-DLY_00Z_',days30,'.nc'))

  #for months with 28days ----------------------------
  years_months28 <- paste0(years,'/',months_28days)
  months28 <- base::sort(rep(months,28))
  months28 <- base::gsub('-', '/', as.character(months28))
  months28 <- months28[months28 %in% years_months28]
  days28 <- paste0(months28,'/',sprintf('%00.2d', 1:28))
  days28 <- base::gsub('/','',as.character(days28))
  days28 <- days28[days28 %in% dates_dw]

  url_28days <- sort(paste0(base_url,tres,'/',sres,'/',months28,'/','CMORPH_V1.0_ADJ_0.25deg-DLY_00Z_',days28,'.nc'))

  #for months with 29days ----------------------------
  years_months29 <- paste0(bic_years,'/',months_28days)
  months29 <- base::sort(rep(months,29))
  months29 <- base::gsub('-', '/', as.character(months29))
  months29 <- months29[months29 %in% years_months29]
  days29 <- paste0(months29,'/',sprintf('%00.2d', 1:29))
  days29 <- base::gsub('/','',as.character(days29))
  days29 <- days29[days29 %in% dates_dw]
  url_29days <- sort(paste0(base_url,tres,'/',sres,'/',months29,'/','CMORPH_V1.0_ADJ_0.25deg-DLY_00Z_',days29,'.nc'))

  unique_years <- as.character(unique(years))

  if (any(bic_years %in% unique_years)) {
    final_urls <- base::sort(c(url_28days,url_29days,url_30days,url_31days))
  } else{
    final_urls <- base::sort(c(url_28days,url_30days,url_31days))
  }

  #running process -------------------------------------

  if (!is.null(roi) & subsetting == TRUE) {

    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    works <- foreach::getDoParWorkers()
    cat('\f')
    cat('running in ',works, 'task for downloading and subsetting',length(final_urls),'netcdf files!\n')

    tictoc::tic('timer')
    foreach::foreach(k=1:length(final_urls),.verbose = verbose)%dopar%{

      destfile <- paste0(outp,basename(final_urls[k]))

      download.file(url = final_urls[k],
                    destfile = destfile,
                    mode = 'wb',
                    method = 'curl')}
    stopCluster(cl)

    files <- base::list.files(path = paste0(dsn,'/download_CMORPH/daily/0.25deg/'), pattern = "*.nc$",all.files=TRUE, full.names= T)
    subsetting_CMORPH.(path_files = files,roi = roi)
    cat('\f')
    cat('----------------------------------------------------------------------\n')
    tictoc::toc(log = TRUE, quiet = TRUE)
    log.txt <- tictoc::tic.log(format = FALSE)
    tictoc::tic.clearlog()
    timings <- unlist(lapply(log.txt, function(x) x$toc - x$tic))
    cat('Time elapsed for download and subsetting', length(files),'netcdf files:', round(timings/60,2),'minutes\n')
    cat('Note that CMORPH prec variable needs to be multiplied by 0.1\n')
  } else {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    works <- foreach::getDoParWorkers()
    cat('\f')
    cat('running in ',works, 'task for downloading and subsetting',length(final_urls),'netcdf files!\n')

    tictoc::tic('timer')
    foreach::foreach(k=1:length(final_urls),.verbose = verbose)%dopar%{

      destfile <- paste0(outp,basename(final_urls[k]))
      utils::download.file(url = final_urls[k],
                           destfile = destfile,
                           mode = 'wb',
                           method = 'curl')
    }

    stopCluster(cl)
    tictoc::toc(log = TRUE, quiet = TRUE)
    log.txt <- tictoc::tic.log(format = FALSE)
    tictoc::tic.clearlog()
    timings <- unlist(lapply(log.txt, function(x) x$toc - x$tic))
    cat('\f')
    cat('Time elapsed for download', length(files),'netcdf files:', round(timings/60,2),'minutes\n')
    message('Note that CMORPH prec variable needs to be multiplied by 0.1\n')
    cat('----------------------------------------------------------------------\n')
  }

}


