################################################################################
#### RAIN4PE
################################################################################

#' @title Functions to download grid available data over PERU.
#'
#' @describeIn Download available data for PERÚ at 0.1 x 0.1 resolution of grid and subsetting to a roi area if require.
#'
#' @param openPDF logical. If \code{TRUE} the PDF of the reference paper will be open. \code{FALSE} just donwload the grid data.
#' @param dsn path. Directory to which you want to store the downloaded data
#' @param name_roi character. Name of the area or basin with which the subsetting to the grid will be done.
#'
#' @import  ncdf4
#' @import  RCurl
#' @import  tictoc
#' @import  glue
#'
#' @family Download grid data
#'
#' @author Crhistian Cornejo
#'
#' @references Fernandez-Palomino, Carlos Antonio; Hattermann, Fred F.; Krysanova, Valentina; Lobanova, Anastasia;
#' Vega-Jácome, Fiorella; Lavado, Waldo; Santini, William; Aybar, Cesar; Bronstert, Axel (2021):
#' Rain for Peru and Ecuador (RAIN4PE). V. 1.0. GFZ Data Services. https://doi.org/10.5880/pik.2020.010
#'
#' @examples
#' \dontrun{
#' ## donwloading data and subsetting for RAIN4PE
#' shp <- 'G:/Mi unidad/MyPackages-R/getSPPs/test/SHPs/cuenca_mayo.shp'
#' download_RAIN4PE(openPDF = FALSE, shp = shp, name_roi = 'Cuenca_Test')
#' }
#' @export


download_RAIN4PE <- function(

  dsn = getwd(),
  openPDF = TRUE,
  shp = NULL,
  name_roi = 'Cuenca_Mayo'

){

  require(ncdf4)

  if (openPDF == TRUE) {
    url_pdf <- 'https://datapub.gfz-potsdam.de/download/10.5880.PIK.2020.010enouiv/2020-010_Fernandes-Palomino_Rain4PE_data-description.pdf'
    browseURL(url_pdf)
  }else{
    print('look PDF paper to see the citation of this product!')
  }

  if (is.null(shp)==FALSE) {
    url_rainfal <- 'https://datapub.gfz-potsdam.de/download/10.5880.PIK.2020.010enouiv/RAIN4PE_daily_0.1d_1981_2015_v1.0.nc.zip'


    outp <- glue::glue(dsn,'/daily/RAIN4PE/')
    ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

    destfile <- paste0(outp,basename(url_rainfal))
    destfile_unzip <- gsub(pattern = ".zip", "", destfile)

    cat('\f')
    cat('downloading data, waiting time depends on your internet speed, be patient!\n')
    download.file(url = url_rainfal,destfile = destfile,
                  mode = 'wb', method = 'curl')

    # to unzip the file
    R.utils::gunzip(destfile,overwrite=T,destname = destfile_unzip)
    # unlink(destfile)

    shp <- rgdal::readOGR(shp)
    shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
    if(!base::class(shp) == 'SpatialPolygonsDataFrame') stop('subs shuold be a SpatialPolygonsDataFrame object')
    if(sp::is.projected(shp)){
      stop('Error: basin dont have projection')
    }  else {
      shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
    }

    ext <- raster::extent(sp::bbox(shp))

    # Reading data
    xmin    <- ext[1]
    xmax    <- ext[2]
    ymin    <- ext[3]
    ymax    <- ext[4]

    # Open netcdf
    nc   <- nc_open(destfile_unzip)

    # First dimension
    if(xmax<0 & xmin<0){
      lon    <- ncvar_get(nc, 'Longitude')
      lonunits <- "degrees_west"
    }else{
      lon  <- ncvar_get(nc, 'Longitude')
      lonunits <- "degrees_east"
    }

    # Second dimension
    if(ymin<0){
      lat      <- ncvar_get(nc, 'Latitude')
      latunits <- "degrees_south"
    }else{
      lat      <- ncvar_get(nc, 'Latitude')
      latunits <- "degrees_north"
    }

    # Third dimension
    time   <- ncvar_get(nc, 'time')
    tunits <- ncatt_get(nc,"time","units")$value
    ntime  <- length(time)
    calendar <- 'standard'

    # variable
    dat      <- ncvar_get(nc, 'pcp')
    units    <- 'mm/day'
    longname <- ncatt_get(nc,"pcp","long_name")$value
    missval  <- ncatt_get(nc,"pcp","_FillValue")$value

    # Subsetting data
    lon_sub <- subset(lon, lon>=xmin & lon<=xmax)
    lat_sub <- subset(lat, lat>=ymin & lat<=ymax)
    dat_sub <- dat[match(lon_sub, lon), match(lat_sub, lat),]

    # Create a new netcdf file
    londim    <- ncdim_def(name="lon",
                           units=lonunits,
                           vals=as.double(lon_sub))
    latdim    <- ncdim_def(name="lat",
                           units=latunits,
                           vals=as.double(lat_sub))
    timedim   <- ncdim_def(name="time",
                           units=tunits,
                           vals=as.double(time),
                           calendar=calendar)
    vardef    <- ncvar_def(name='pcp',
                           units=units,
                           dim=list(londim,latdim,timedim),
                           missval=missval,
                           longname=longname,
                           prec="float")

    filename <- paste0('RAIN4PE_daily_0.1d_1981_2015_v1.0_',name_roi,'.nc')
    gsub     <- substr(outp, 1,nchar(outp)-1)
    destfile <- file.path(gsub,filename)

    ncnew    <- nc_create(filename=destfile,
                          vars=list(vardef),
                          force_v4=TRUE)
    ncvar_put(nc=ncnew,
              varid=vardef,
              vals=dat_sub)
    nc_close(nc)
    nc_close(ncnew)
    unlink(destfile)
    gc(reset=TRUE)

  } else{
    # Download original data for PERU
    url_rainfal <- 'https://datapub.gfz-potsdam.de/download/10.5880.PIK.2020.010enouiv/RAIN4PE_daily_0.1d_1981_2015_v1.0.nc.zip'
    outp <- glue::glue(dsn,'/daily/RAIN4PE/')
    ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

    destfile <- paste0(outp,basename(url_rainfal))
    destfile_unzip <- gsub(pattern = ".zip", "", destfile)

    cat('\f')
    cat('downloading data, waiting time depends on your internet speed, be patient!\n')
    download.file(url = url_rainfal,destfile = destfile,
                  mode = 'wb', method = 'curl')

    # to unzip the file
    R.utils::gunzip(destfile,overwrite=T,destname = destfile_unzip)
    gc(reset=T)

  }

} #end of function!



################################################################################
#### PISCO
################################################################################

#' @title Download PISCO grid daily and monthly data and subsetting for a basin extend.
#' @describeIn Download RAIN4PE grid at daily scale, PISCO at daily and monthly grid data and subsetting for a roi extend.
#'
#' @param var character. variables that will be downloaded by the function, \code{c('Prec', 'PET', 'Temp')} are available.
#' @param dsn path. Directory to which you want to store the downloaded data.
#' @param tres character. Temporal resolution. \code{"daily"} or \code{"monthly"}.
#' @param version character. For now just 'stable' version is available.
#' @param shp Path where the shapefile is alocate. Keep on \code{NULL} for download hourly data.
#' @param name_roi character. Name of the area or basin with which the subsetting to the grid will be done.
#' @import  ncdf4
#' @import  RCurl
#' @import  tictoc
#' @import  glue
#'
#'
#' @author Crhistian Cornejo
#'
#' @references Aybar, C., Fernández, C., Huerta, A., Lavado, W., Vega, F., & Felipe-Obando, O.G. (2019).
#' Construction of a high-resolution gridded rainfall dataset for Peru from 1981 to the present day.
#' Hydrological Sciences Journal, 65, 770 - 785.
#'
#' @examples
#' \dontrun{
#' ## donwloading data and subsetting for PISCO
#' var <- c('Prec', 'PET', 'Temp')
#' shp <- 'G:/Mi unidad/MyPackages-R/getSPPs/test/SHPs/cuenca_mayo.shp'
#' download_PISCO(var = var[1], dsn = getwd(), shp = shp, tres = 'monthly', version = 'stable')
#' }
#' @export


download_PISCO <- function(

  var = c('Prec', 'PET', 'Temp'),
  dsn = getwd(),
  tres = c('monthly','daily', 'hourly'),
  version = 'stable',
  shp = NULL,
  name_roi = NULL
){
  require(ncdf4)

  ##web scrapping to download PISCO data
  base_url <- 'https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/'

  if (var == 'Prec') {

    update_url <- paste0(base_url,'.Prec/.v2p1/','.',version,'/.',tres,'/.',var,'/data.nc')

    if (is.null(shp) == FALSE) {

      shp <- rgdal::readOGR(shp)
      shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
      if(!base::class(shp) == 'SpatialPolygonsDataFrame') stop('subs shuold be a SpatialPolygonsDataFrame object')
      if(sp::is.projected(shp)){
        stop('Error: basin dont have projection')
      }  else {
        shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
      }

      ext <- raster::extent(sp::bbox(shp))

      # Reading data
      xmin    <- ext[1]
      xmax    <- ext[2]
      ymin    <- ext[3]
      ymax    <- ext[4]

      #download

      outp <- glue::glue(dsn,'/download_PISCO/{var}/{tres}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

      destfile <- paste0(outp,basename(update_url))

      cat('\f')
      message('Starting the download for ',var,' in ',tres,' temportal resolution')
      download.file(url = update_url,
                    destfile = destfile,
                    mode = 'wb',
                    method = 'curl')

      new_name <- gsub('data.nc', paste0('PISCO','_',tres,'_',var,'_v2p1.nc'), as.character(destfile))
      # rename it
      file.rename(from = destfile, to = new_name)
      gc(reset=T)

      # Open netcdf
      nc   <- nc_open(new_name)

      # First dimension
      lon      <- ncvar_get(nc, 'X')
      lonunits <- "degree_east"

      # Second dimension
      lat      <- ncvar_get(nc, 'Y')
      latunits <- "degrees_north"

      # Third dimension
      time   <- ncvar_get(nc, 'T')
      tunits <- ncatt_get(nc,'T','units')$value
      ntime  <- length(time)
      calendar <- 'standard'

      # variable
      dat      <- ncvar_get(nc, 'Prec')
      units    <- 'mm/day'
      longname <- ncatt_get(nc,"Prec","long_name")$value
      missval  <- ncatt_get(nc,"Prec","missing_value")$value

      # Subsetting data
      lon_sub <- subset(lon, lon>=xmin & lon<=xmax)
      lat_sub <- subset(lat, lat>=ymin & lat<=ymax)
      dat_sub <- dat[match(lon_sub, lon), match(lat_sub, lat),]

      # Create a new netcdf file
      londim    <- ncdim_def(name="lon",
                             units=lonunits,
                             vals=as.double(lon_sub))
      latdim    <- ncdim_def(name="lat",
                             units=latunits,
                             vals=as.double(lat_sub))
      timedim   <- ncdim_def(name="time",
                             units=tunits,
                             vals=as.double(time),
                             calendar=calendar)
      vardef    <- ncvar_def(name='pcp',
                             units=units,
                             dim=list(londim,latdim,timedim),
                             missval=missval,
                             longname=longname,
                             prec="float")

      filename <- paste0(gsub('.nc','_',as.character(new_name)),name_roi,'.nc')
      ncnew    <- nc_create(filename=filename,
                            vars=list(vardef),
                            force_v4=TRUE)
      ncvar_put(nc=ncnew,
                varid=vardef,
                vals=dat_sub)
      nc_close(nc)
      nc_close(ncnew)
      gc(reset=TRUE)
      cat('\f')
      cat('-------- Finish donwload and subsetting!! -----------\n')


    } else{

      shp <- shp <- NULL

      update_url <- paste0(base_url,'.Prec/.v2p1/','.',version,'/.',tres,'/.',var,'/data.nc')
      outp <- glue::glue(dsn,'/download_PISCO/{var}/{tres}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

      destfile <- paste0(outp,basename(update_url))

      download.file(url = update_url,
                    destfile = destfile,
                    mode = 'wb',
                    method = 'curl')

      new_name <- gsub('data.nc', paste0('PISCO','_',tres,'_',var,'_v2p1.nc'), as.character(destfile))
      # rename it
      file.rename(from = destfile, to = new_name)
      gc(reset=T)

      if (tres == 'hourly') {

        url <- 'https://figshare.com/ndownloader/articles/17148401/versions/2' #from Andrian Huerta fishared page
        outp <- glue::glue(dsn,'/download_PISCO/{var}/{tres}/')
        ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

        destfile <- paste0(outp,'PISCO_hourly','.zip')

        cat('\f')
        message('---------- Downloading hourly grid data from PISCO precipitation -------------')
        download.file(url = url,
                      destfile = destfile,
                      mode = 'wb',
                      method = 'curl')

        # unziped_names <- grep('\\.nc$', utils::unzip(destfile, list=TRUE)$Name, ignore.case=TRUE, value=TRUE)
        utils::untar(destfile, exdir = outp)

        file.remove(destfile)
        gc(reset=TRUE)
        cat('\f')
        cat('-------- Finish download for Hourly grid data!! -----------\n')


      } #end hourly conditional

    } #end precipitation conditional else, where if shp is NULL will donwload all original data

  } #aqui termina el primer if para precipitacion

  # star Potential Evapotranspiration -----------------------------------------------------------------------------
  if (var == 'PET') {

    if (is.null(shp) == FALSE) {

      shp <- rgdal::readOGR(shp)
      shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
      if(!base::class(shp) == 'SpatialPolygonsDataFrame') stop('subs shuold be a SpatialPolygonsDataFrame object')
      if(sp::is.projected(shp)){
        stop('Error: basin dont have projection')
      }  else {
        shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
      }

      ext <- raster::extent(sp::bbox(shp))

      # Reading data
      xmin    <- ext[1]
      xmax    <- ext[2]
      ymin    <- ext[3]
      ymax    <- ext[4]

      #donwload

      url_PET <- 'https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.PET/.v1p1/.stable/'
      update_url <- paste0(url_PET,'.',tres,'/.',var,'/data.nc')

      outp <- glue::glue(dsn,'/download_PISCO/{var}/{tres}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

      destfile <- paste0(outp,basename(update_url))

      cat('\f')
      message('Starting the download for ',var,' in ',tres,' temportal resolution')
      download.file(url = update_url,
                    destfile = destfile,
                    mode = 'wb',
                    method = 'curl')

      new_name <- gsub('data.nc', paste0('PISCO','_',tres,'_',var,'_v1p1.nc'), as.character(destfile))
      # rename it
      file.rename(from = destfile, to = new_name)
      gc(reset=T)

      # Open netcdf for subsetting
      nc   <- nc_open(new_name)

      # First dimension
      lon      <- ncvar_get(nc, 'X')
      lonunits <- "degree_east"

      # Second dimension
      lat      <- ncvar_get(nc, 'Y')
      latunits <- "degrees_north"

      # Third dimension
      time   <- ncvar_get(nc, 'T')
      tunits <- ncatt_get(nc,'T','units')$value
      ntime  <- length(time)
      calendar <- 'standard'

      # variable
      dat      <- ncvar_get(nc, 'PET')
      units    <- 'mm/day'
      longname <- ncatt_get(nc,"PET","long_name")$value
      missval  <- ncatt_get(nc,"PET","missing_value")$value

      # Subsetting data
      lon_sub <- subset(lon, lon>=xmin & lon<=xmax)
      lat_sub <- subset(lat, lat>=ymin & lat<=ymax)
      dat_sub <- dat[match(lon_sub, lon), match(lat_sub, lat),]

      # Create a new netcdf file
      londim    <- ncdim_def(name="lon",
                             units=lonunits,
                             vals=as.double(lon_sub))
      latdim    <- ncdim_def(name="lat",
                             units=latunits,
                             vals=as.double(lat_sub))
      timedim   <- ncdim_def(name="time",
                             units=tunits,
                             vals=as.double(time),
                             calendar=calendar)
      vardef    <- ncvar_def(name='pcp',
                             units=units,
                             dim=list(londim,latdim,timedim),
                             missval=missval,
                             longname=longname,
                             prec="float")

      filename <- paste0(gsub('.nc','_',as.character(new_name)),name_roi,'.nc')
      ncnew    <- nc_create(filename=filename,
                            vars=list(vardef),
                            force_v4=TRUE)
      ncvar_put(nc=ncnew,
                varid=vardef,
                vals=dat_sub)
      nc_close(nc)
      nc_close(ncnew)
      gc(reset=TRUE)
      cat('\f')
      cat('-------- Finish donwload and subsetting!! -----------\n')
    } else {#end for conditional shp on PET variable

      url_PET <- 'https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.PET/.v1p1/.stable/'
      update_url <- paste0(url_PET,'.',tres,'/.',var,'/data.nc')

      outp <- glue::glue(dsn,'/download_PISCO/{var}/{tres}/')
      ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

      destfile <- paste0(outp,basename(update_url))

      cat('\f')
      message('Starting the download for ',var,' in ',tres,' temportal resolution')
      download.file(url = update_url,
                    destfile = destfile,
                    mode = 'wb',
                    method = 'curl')

      new_name <- gsub('data.nc', paste0('PISCO','_',tres,'_',var,'_v1p1.nc'), as.character(destfile))
      # rename it
      file.rename(from = destfile, to = new_name)
      gc(reset=T)
      cat('\f')
      cat('-------- Finish download PET grid data!! -----------\n')

    } #end of else conditional for PET
  } #end of PET conditional general

  # star Temperature -----------------------------------------------------------------------------

  if (var == 'Temp') {

    if (is.null(shp)==FALSE) {

      shp <- rgdal::readOGR(shp)
      shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
      if(!base::class(shp) == 'SpatialPolygonsDataFrame') stop('subs shuold be a SpatialPolygonsDataFrame object')
      if(sp::is.projected(shp)){
        stop('Error: basin dont have projection')
      }  else {
        shp <- sp::spTransform(shp, sp::CRS("+proj=longlat +datum=WGS84 +no_defs "))
      }

      ext <- raster::extent(sp::bbox(shp))

      # Reading data
      xmin    <- ext[1]
      xmax    <- ext[2]
      ymin    <- ext[3]
      ymax    <- ext[4]

      #donwloading data
      vars <- c('tmax', 'tmin')
      outp <- glue::glue(dsn,'/download_PISCO/{var}/{tres}/{vars}/')


      for (i in 1:length(vars)) {
        ifelse(!dir.exists(outp[i]),xfun::dir_create(outp[i]),print('already exist!'))
        # ifelse(!dir.exists(outp[2]),xfun::dir_create(outp[2]),print('already exist!'))
        update_url[i] <- paste0(base_url,'.Temp/.v1p1/','.',vars[i],'/.',version,'/.',tres,'/.',vars[i],'/data.nc')
        cat('\f')
        cat('--------------------------------------------------------------------------\n')
        cat('Downloading',vars[i],'from PISCO product on',tres,'temporal resolution\n')
        cat('--------------------------------------------------------------------------\n')
        destfile <- paste0(outp,basename(update_url))
        download.file(url = update_url[i],
                      destfile = destfile[i],
                      mode = 'wb',
                      method = 'curl')



        new_name[i] <- gsub('data.nc', paste0('PISCO','_',tres,'_',vars[i],'_v1p1.nc'), as.character(destfile[i]))


        # rename it
        file.rename(from = destfile[i], to = new_name[i])

        # Open netcdf

        nc   <- nc_open(new_name[i])

        # First dimension
        lon      <- ncvar_get(nc, 'X')
        lonunits <- "degree_east"

        # Second dimension
        lat      <- ncvar_get(nc, 'Y')
        latunits <- "degrees_north"

        # Third dimension
        time   <- ncvar_get(nc, 'T')
        tunits <- ncatt_get(nc,'T','units')$value
        ntime  <- length(time)
        calendar <- 'standard'

        # variable
        dat      <- ncvar_get(nc, vars[i])
        units    <- 'Celsius_scale'
        longname <- ncatt_get(nc,vars[i],"long_name")$value
        missval  <- ncatt_get(nc,vars[i],"missing_value")$value

        # Subsetting data
        lon_sub <- subset(lon, lon>=xmin & lon<=xmax)
        lat_sub <- subset(lat, lat>=ymin & lat<=ymax)
        dat_sub <- dat[match(lon_sub, lon), match(lat_sub, lat),]

        # Create a new netcdf file
        londim    <- ncdim_def(name="lon",
                               units=lonunits,
                               vals=as.double(lon_sub))
        latdim    <- ncdim_def(name="lat",
                               units=latunits,
                               vals=as.double(lat_sub))
        timedim   <- ncdim_def(name="time",
                               units=tunits,
                               vals=as.double(time),
                               calendar=calendar)
        vardef    <- ncvar_def(name=vars[i],
                               units=units,
                               dim=list(londim,latdim,timedim),
                               missval=missval,
                               longname=longname,
                               prec="float")

        filename <- paste0(gsub('.nc','_',as.character(new_name[i])),name_roi,'.nc')
        ncnew    <- nc_create(filename=filename,
                              vars=list(vardef),
                              force_v4=TRUE)
        ncvar_put(nc=ncnew,
                  varid=vardef,
                  vals=dat_sub)
        nc_close(nc)
        nc_close(ncnew)
        gc(reset=TRUE)

      }

      cat('\f')
      message('-------- Finish download and subsetting ',tres,' grid data for Tmax and Tmin!! -----------')


    } else{

      vars <- c('tmax', 'tmin')
      outp <- glue::glue(dsn,'/download_PISCO/{var}/{tres}/{vars}/')

      for (i in 1:length(vars)) {
        ifelse(!dir.exists(outp[i]),xfun::dir_create(outp[i]),print('already exist!'))
        update_url[i] <- paste0(base_url,'.Temp/.v1p1/','.',vars[i],'/.',version,'/.',tres,'/.',vars[i],'/data.nc')
        cat('\f')
        cat('--------------------------------------------------------------------------\n')
        cat('Downloading',vars[i],'from PISCO product on',tres,'temporal resolution\n')
        cat('--------------------------------------------------------------------------\n')
        destfile <- paste0(outp,basename(update_url))
        download.file(url = update_url[i],
                      destfile = destfile[i],
                      mode = 'wb',
                      method = 'curl')

        new_name[i] <- gsub('data.nc', paste0('PISCO','_',tres,'_',vars[i],'_v1p1.nc'), as.character(destfile[i]))


        # rename it
        file.rename(from = destfile[i], to = new_name[i])

        gc(reset=T)

        cat('\f')
        message('-------- Finish download Tmin and Tmax ',tres,' original grid data!! -----------')

      } #end for
    }#end function for TEMPERATURE
  }
}#end function!


# shp <- 'G:/Mi unidad/MyPackages-R/eClimTools/Data/SHPs/cuenca_mayo.shp'

# download_PISCO(var = 'Temp',
#                dsn = getwd(),
#                tres = 'daily',
#                version = 'stable',
#                shp = shp,
#                name_roi = 'Subsetting')


