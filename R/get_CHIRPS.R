################################################################################
#### FUNCTIONS TO GET CHIRPS PROUCT WITH SUBSETTING EXTENSION
################################################################################

#' @title Function to download CHIRPS raster data global on daily and monthly scales
#'
#' @param tres character. Temporal resolution. \code{"daily"} or \code{"monthly"}
#' @param sres numeric. Spatial resolution. \code{0.05} or \code{0.25}. Note
#' that monthly data is only available at a resolution of 0.05 degrees.
#' @param years numeric. Years Date(s) for which you want to download CHIRPS data.
#' @param dsn path. Directory to which you want to store the downloaded data.
#' @param format character. Download format of the files. \code{"tif"} or \code{"netcdf"}
#' are only available.
#' @param cores numeric. Number of cores to be used for the download. By default,
#' it automatically detects how many cores there are on your computer and
#' subtracts 1 to prevent it from slowing down.
#' @param verbose logical. If TRUE this will show the task process for the parallel process.
#' @param roi numeric. Vector with xmax, xmin, ymax, ymin values for a region for subsetting the data.
#'
#' @import  ncdf4
#' @import  RCurl
#' @import  tictoc
#' @import  glue
#' @import  foreach
#' @import  doParallel
#'
#' @author Crhistian Cornejo
#'
#' @references https://data.chc.ucsb.edu/products/CHIRPS-2.0/
#'
#' @examples
#' \dontrun{
#' # roi = c(-86,-66,-20,2)
#' # download_CHIRPS(tres = 'daily',sres = 0.25,format = 'tif',years = 2000,roi = roi)
#' }
#'
#' @export

download_CHIRPS <- function(

  tres  = NULL,
  sres  = NULL,
  years = 1981,
  dsn   = getwd(),
  format = NULL,
  cores = parallel::detectCores()-1,
  verbose = FALSE,
  roi= c(-86,-66,-20,2)){

  #validating vars
  options (warn = - 1)

  sres <- sres
  format <- tolower(format)
  dsn <- dsn
  years <- years
  cores <- cores
  verbose <- verbose
  tres <- tolower(tres)
  roi <- roi

  # require(foreach)
  # require(doParallel)
  # require(dplyr)

  #setting error's messages
  if (is.null(sres) & tres == 'daily') {
    stop("for daily download, please specify the spatial resolution param: 'sres'\n")
  }

  ## conditionals for the function!
  if (tres == 'daily') {

    if (sres == 0.25 & format == 'netcdf'){

      outp <- glue::glue(dsn,'/daily/{sres}/{format}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('downloading...'))
      paths <- paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/chirps-v2.0.',
                      years,'.days_p05.nc')
      cl <- makeCluster(cores, type='SOCK')
      registerDoParallel(cl)

      cat('\f')
      cat('running in ',cores, 'task for downloading',length(paths),format,'files\n')
      tictoc::tic()
      foreach(k=1:length(paths),.verbose = verbose)%dopar%{

        download.file(url = paths[k],
                      destfile = paste0(outp,basename(paths[k])),
                      mode = 'wb', method = 'curl')
      }
      stopCluster(cl)
      tictoc::toc()

    }

    if (sres == 0.05 & format == 'netcdf'){

      base_url <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/'
      outp <- glue::glue(dsn,'/daily/{sres}/{format}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('downloading...'))
      paths <- paste0(base_url,'chirps-v2.0.',years,'.days_p05.nc')

      cl <- makeCluster(cores, type='SOCK')
      registerDoParallel(cl)


      cat('\f')
      cat('running in ',cores, 'task for downloading',length(paths),'files\n')
      cat('this is equivalent to a stack for each year!')
      tictoc::tic()
      foreach(k=1:length(paths),.verbose = verbose)%dopar%{

        download.file(url = paths[k],
                      destfile = paste0(outp,basename(paths[k])),
                      mode = 'wb', method = 'curl')

      }
      stopCluster(cl)
      tictoc::toc()

    }

    if (format == 'tif') {

      if (sres == 0.05) {

        base_url <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/'
        outp <- glue::glue(dsn,'/daily/{sres}/{format}/')
        ifelse(!dir.exists(outp),xfun::dir_create(outp),print('downloading...'))
        start_year <- head(years,1)
        last_year <- tail(years,1)
        dates <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(last_year, "-12-31")), by="days")
        dates <- gsub('-', '.', as.character(dates))
        paths <- paste0(base_url, years, '/chirps-v2.0.', dates, '.tif.gz')

        #runing parallel
        cat('\f')
        cat('running in ',cores, 'task for downloading, unziped and subsetting',length(paths),format,'files\n')
        cl <- parallel::makeCluster(cores, type='SOCK')
        doParallel::registerDoParallel(cl)
        tictoc::tic()
        foreach(k=1:length(paths),.verbose = verbose)%dopar%{

          destfile <- paste0(outp,basename(paths[k]))
          destfile_unzip <- gsub(pattern = ".gz", "", destfile)
          download.file(url = paths[k],
                        destfile = destfile,
                        mode = 'wb')

          R.utils::gunzip(destfile,overwrite=T,destname = destfile_unzip)

        }
        stopCluster(cl)

        # subseting tif files to be clipped

        files <- base::list.files(path = paste0(dsn,'/daily/0.05/tif/'), pattern = "*.tif$",all.files=TRUE, full.names=T)
        ex <- raster::extent(roi[1], roi[2], roi[3], roi[4])

        sapply(files, function(f) {
          gdalUtilities::gdal_translate(f, sub('\\.tif', '_clipped.tif', f),
                                        r = 'bilinear',
                                        projwin=c(raster::xmin(ex),
                                                  raster::ymax(ex),
                                                  raster::xmax(ex),
                                                  raster::ymin(ex)))
        })
        for (t in 1:length(files)){unlink(files[t])}

        tictoc::toc()
        cat('--------- Finished the download and subsetting to the roi extension!!! ----------\n')

      }

      if (sres == 0.25) {

        base_url <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p25/'
        outp <- glue::glue(dsn,'/daily/{sres}/{format}/')
        ifelse(!dir.exists(outp),xfun::dir_create(outp),print('downloading...'))
        start_year <- head(years,1)
        last_year <- tail(years,1)
        dates <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(last_year, "-12-31")), by="days")
        dates <- gsub('-', '.', as.character(dates))
        paths <- paste0(base_url, years, '/chirps-v2.0.', dates, '.tif.gz')

        #runing parallel
        cat('\f')
        cat('running in ',cores, 'task for downloading, unziped and subsetting',length(paths),format,'files\n')
        cl <- parallel::makeCluster(cores, type='SOCK')
        doParallel::registerDoParallel(cl)
        tictoc::tic()
        foreach(k=1:length(paths),.verbose = verbose)%dopar%{

          destfile <- paste0(outp,basename(paths[k]))
          destfile_unzip <- gsub(pattern = ".gz", "", destfile)
          download.file(url = paths[k],
                        destfile = destfile,
                        mode = 'wb')


          R.utils::gunzip(destfile,overwrite=T,destname = destfile_unzip)

        }
        stopCluster(cl)

        # subseting tif files to be clipped

        files <- base::list.files(path = paste0(dsn,'/daily/0.25/tif/'), pattern = "*.tif$",all.files=TRUE, full.names=T)
        ex <- raster::extent(roi[1], roi[2], roi[3], roi[4])

        sapply(files, function(f) {
          gdalUtilities::gdal_translate(f, sub('\\.tif', '_clipped.tif', f),
                                        r = 'bilinear',
                                        projwin=c(raster::xmin(ex),
                                                  raster::ymax(ex),
                                                  raster::xmax(ex),
                                                  raster::ymin(ex)))
        })
        for (t in 1:length(files)){unlink(files[t])}

        tictoc::toc()
        cat('--------- Finished the download and subsetting to the roi extension!!! ----------\n')
      }

    }

  }

  #for monthly request
  if (tres == 'monthly') {

    if (format == 'netcdf'){

      path <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc'
      outp <- glue::glue(dsn,'/monthly/{format}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('downloading...'))

      for (i in 1:length(path)) {
        cat('\f')
        cat('--------------------------------------------------------------\n')
        cat('This will take some time!!, downloading only one file: 6.54 gb\n')
        cat('--------------------------------------------------------------\n')
        download.file(url = path[i],
                      destfile = paste0(outp,basename(path[i])),
                      mode = 'wb',quiet = TRUE,cacheOK = T)
        cat('----------------- FINISH!! --------------------\n')
      }

    }

    if (format == 'tif'){

      base_url <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/'
      outp <- glue::glue(dsn,'/monthly/{format}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('downloading...'))

      #setting variables for run this
      start_year <- head(years,1)
      last_year <- tail(years,1)
      dates <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(last_year, "-12-31")), by="month")
      dates <- gsub('-', '.', as.character(dates))
      dates <- substr(dates, 1,nchar(dates)-3)
      paths <- paste0(base_url,'chirps-v2.0.',dates,'.tif.gz')

      #runing parallel
      cat('\f')
      cat('running in ',cores, 'task for downloading, unziped and subsetting',length(paths),format,'files\n')
      cl <- parallel::makeCluster(cores, type='SOCK')
      doParallel::registerDoParallel(cl)
      tictoc::tic()
      foreach(k=1:length(paths),.verbose = verbose)%dopar%{

        destfile <- paste0(outp,basename(paths[k]))
        destfile_unzip <- gsub(pattern = ".gz", "", destfile)
        download.file(url = paths[k],
                      destfile = destfile,
                      mode = 'wb')


        R.utils::gunzip(destfile,overwrite=T,destname = destfile_unzip)

      }
      stopCluster(cl)

      # subseting tif files to be clipped

      files <- base::list.files(path = paste0(dsn,'/monthly/tif/'), pattern = "*.tif$",all.files=TRUE, full.names=T)
      ex <- raster::extent(roi[1], roi[2], roi[3], roi[4])

      sapply(files, function(f) {
        gdalUtilities::gdal_translate(f, sub('\\.tif', '_clipped.tif', f),
                                      r = 'bilinear',
                                      projwin=c(raster::xmin(ex),
                                                raster::ymax(ex),
                                                raster::xmax(ex),
                                                raster::ymin(ex)))
      })
      for (t in 1:length(files)){unlink(files[t])}

      tictoc::toc()
      cat('--------- Finished the download and subsetting to the roi extension!!! ----------\n')
    }

  }

} #end of whole function!



