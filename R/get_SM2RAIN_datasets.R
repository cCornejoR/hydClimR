######################################
#### FUNCTION TO GET SM2RAIN Data sets
######################################

#' @title Function to download SM2RAIN global netcdf datasets on daily sacale.
#'
#' @param dsn character. Path where the output folders and files will be created.
#' @param data_set character. SM2RAIN data sets available for download: c('SM2RAIN-ASCAT','GPM+SM2RAIN','SM2RAIN-CCI'). for
#' \code{SM2RAIN-ASCAT} data available from 2007-2021, \code{GPM+SM2RAIN} data available from 2007-2018,
#' \code{SM2RAIN-CCI} data available from 1998-2015,
#' @param cores numeric. Number of cores to be used for the download. By default,
#' it automatically detects how many cores there are on your computer and
#' subtracts 1 to prevent it from slowing down.
#' @param years numeric. Years Date(s) for which you want to download the data.
#'
#' @import  ncdf4
#' @import  RCurl
#' @import  tictoc
#' @import  glue
#' @import  foreach
#' @import  doParallel
#'
#' @family Download grid data
#'
#' @author Crhistian Cornejo
#'
#' @references
#' Brocca, L., Filippucci, P., Hahn, S., Ciabatta, L., Massari, C., Camici, S., Schüller, L., Bojkov, B., and Wagner, W. (2019).
#' SM2RAIN-ASCAT (2007–2018): global daily satellite rainfall from ASCAT soil moisture, Earth Syst. Sci. Data Discuss.,
#' https://doi.org/10.5194/essd-2019-48, in review.
#'
#' Brocca, L., Ciabatta, L., Massari, C., Moramarco, T., Hahn, S., Hasenauer, S., Kidd, R., Dorigo, W., Wagner, W., Levizzani, V. (2014).
#' Soil as a natural rain gauge: estimating global rainfall from satellite soil moisture data. Journal of Geophysical Research, 119(9),
#' 5128-5141, doi:10.1002/2014JD021489.
#'
#' Wagner, W., Hahn, S., Kidd, R., Melzer, T., Bartalis, Z., Hasenauer, S., Figa, J., de Rosnay, P., Jann, A., Schneider, S.,
#' Komma, J., Kubu, G., Brugger, K., Aubrecht, C., Zuger, J., Gangkofner, U., Kienberger, S., Brocca, L., Wang, Y., Bloeschl,
#' G., Eitzinger, J., Steinnocher, K., Zeil, P., Rubel, F. (2013). The ASCAT Soil Moisture Product: A Review of its Specifications,
#' Validation Results, and Emerging Applications. Meteorologische Zeitschrift, 22(1), 5-33, doi:10.1127/0941-2948/2013/0399.
#'
#' @examples
#' \dontrun{
#'download_SM2RAIN(data_set = 'SM2RAIN-ASCAT',
#'                 years = 2007:2010)
#' }
#'
#' @export
#'

download_SM2RAIN <- function(

  dsn = getwd(),
  data_set = c('SM2RAIN-ASCAT','GPM+SM2RAIN','SM2RAIN-CCI'),
  years = 2007,
  cores = parallel::detectCores()-1

){
  options(warn = - 1)

  if (data_set == 'SM2RAIN-ASCAT') {

    url <- paste0('https://zenodo.org/record/6136294/files/SM2RAIN_ASCAT_0125_',years,'_v1.5.nc?download=1')
    outp <- glue::glue(dsn,'/download_SM2RAIN/{data_set}/')
    ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)

    cat('\f')
    message('running download in ',cores, ' task for ',length(url),' netcdf files!\n')

    tictoc::tic('timer')
    foreach::foreach (k = 1:length(url)) %dopar% {
      destfile[k] <- base::paste0(outp,basename(url[k]))
      destfile[k] <- substr(destfile[k],1,nchar(destfile[k])-11)

      download.file(url = url[k],
                    destfile = destfile[k],
                    mode = 'wb', method = 'curl')
    }

    stopCluster(cl)
    tictoc::toc(log = TRUE, quiet = TRUE)
    log.txt <- tictoc::tic.log(format = FALSE)
    tictoc::tic.clearlog()
    timings <- unlist(lapply(log.txt, function(x) x$toc - x$tic))

    cat('\f')
    cat('----------------------------------------------------------\n')
    message('Time elapsed for download ', length(url),' netcdf files: ', round(timings/60,2),' minutes\n')
    cat('----------------------------------------------------------\n')

  }

  if (data_set == 'GPM+SM2RAIN') {

    url <- paste0('https://zenodo.org/record/3854817/files/SM2RAIN_SMOS_RAINFALL_025_',years,'.nc?download=1')
    outp <- glue::glue(dsn,'/download_SM2RAIN/{data_set}/')
    ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)

    cat('\f')
    message('running download in ',cores, ' task for ',length(url),' netcdf files!\n')

    tictoc::tic('timer')
    foreach::foreach (k = 1:length(url)) %dopar% {
      destfile[k] <- base::paste0(outp,basename(url[k]))
      destfile[k] <- substr(destfile[k],1,nchar(destfile[k])-11)

      download.file(url = url[k],
                    destfile = destfile[k],
                    mode = 'wb', method = 'curl')
    }

    stopCluster(cl)
    tictoc::toc(log = TRUE, quiet = TRUE)
    log.txt <- tictoc::tic.log(format = FALSE)
    tictoc::tic.clearlog()
    timings <- unlist(lapply(log.txt, function(x) x$toc - x$tic))

    cat('\f')
    cat('----------------------------------------------------------\n')
    message('Time elapsed for download ', length(url),' netcdf files: ', round(timings/60,2),' minutes\n')
    cat('----------------------------------------------------------\n')

  }

  if (data_set == 'SM2RAIN-CCI') {

    url <- paste0('https://zenodo.org/record/1305021/files/CCI_SM2RAIN_',years,'_BC.nc?download=1')
    outp <- glue::glue(dsn,'/download_SM2RAIN/{data_set}/')
    ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)

    cat('\f')
    message('running download in ',cores, ' task for ',length(url),' netcdf files!\n')

    tictoc::tic('timer')
    foreach::foreach (k = 1:length(url)) %dopar% {
      destfile[k] <- base::paste0(outp,basename(url[k]))
      destfile[k] <- substr(destfile[k],1,nchar(destfile[k])-11)

      download.file(url = url[k],
                    destfile = destfile[k],
                    mode = 'wb', method = 'curl')
    }

    stopCluster(cl)
    tictoc::toc(log = TRUE, quiet = TRUE)
    log.txt <- tictoc::tic.log(format = FALSE)
    tictoc::tic.clearlog()
    timings <- unlist(lapply(log.txt, function(x) x$toc - x$tic))

    cat('\f')
    cat('----------------------------------------------------------\n')
    message('Time elapsed for download ', length(url),' netcdf files: ', round(timings/60,2),' minutes\n')
    cat('----------------------------------------------------------\n')

  }

}


