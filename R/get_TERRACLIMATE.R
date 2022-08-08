################################################################################
#### Donwload TERRACLIMATE data sets
################################################################################

#' @title Download global individual TERRACLIMATE data sets at 4km resolution.
#'
#' @description Download 14 climatic variables simultaneously from the TERRACLIMATE database at 4km resolution.
#' @param vars_id character. Name of variables what you want to download. for DEFAULT it download all
#' variables available.
#' @param years integer. Years from which you want to download the information, by DEFAULT it downloads all the
#' available years of TERRACLIMATE of all the variables.
#' @param dsn character. Path where the output folders and files will be saved.
#' @param cores numeric. Necessary parameter to download multiple variables in parallel.
#' @references
#' Abatzoglou, J.T., S.Z. Dobrowski, S.A. Parks, K.C. Hegewisch, 2018, Terraclimate,
#' a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015, Scientific Data,
#'
#' @return Donwload TERRACLIMATE data sets to output directory.

#' @examples
#' \dontrun{
#' if(interactive()){
#' download_TERRACLIMATE(vars_id  = c('aet','ppt'), years = 2000:2002, cores = 4)
#'  }
#' }

#' @rdname download_TERRACLIMATE
#' @export
#' @importFrom parallel detectCores makeCluster
#' @importFrom tictoc tic toc tic.log tic.clearlog
#' @importFrom glue glue
#' @importFrom xfun dir_exists dir_create
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#'


download_TERRACLIMATE <- function(

  vars_id  = c('aet','def','swe','q','soil','PDSI', 'pet','ppt','srad','tmax','tmin','vap','vpd','ws'),
  years = 1958:2021,
  dsn = getwd(),
  cores = parallel::detectCores()-1

){

  options(scipen = 999, warn = -1)
  tictoc::tic('timer')
  base_url <- 'https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate'

  for (i in 1:length(years)) {
    link <- sort(paste0(base_url,'_',rep(vars_id,i),'_',years,'.nc'))
  }

  outp <- glue::glue('{dsn}/download_TERRACLIMATE/Individual_years/')
  for (j in 1:length(outp)) {
    ifelse(!xfun::dir_exists(outp[j]), xfun::dir_create(outp[j], recursive = TRUE), print('Folder exists'))
  }

  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  cat('\f')
  if (cores < length(link)) {
    message('running download in ',cores, ' task for ',length(link),' TERRACLIMATE netcdf files!\n')
  }else{message('Starting download...')}

  destfile <- base::paste0(outp,basename(link))

  foreach::foreach(k = 1:length(link)) %dopar% {
    download.file(url = link[k],
                  destfile = destfile[k],
                  mode = 'wb',
                  method = 'curl')
  }

  stopCluster(cl)
  tictoc::toc(log = TRUE, quiet = TRUE)
  log.txt <- tictoc::tic.log(format = FALSE)
  tictoc::tic.clearlog()
  timings <- unlist(lapply(log.txt, function(x) x$toc - x$tic))

  cat('\f')
  cat('----------------------------------------------------------\n')
  message('Time elapsed for download ', length(link),' netcdf files: ', round(timings/60,2),' minutes\n')
  cat('----------------------------------------------------------\n')

}


################################################################################
#### Subsetting TERRACLIMATE data sets
################################################################################

#' @title Function to subset and aggregate TERRACLIMATE netcdfs and exported the stack as tif.
#' @description Download 14 climatic variables simultaneously from the TERRACLIMATE database at 4km resolution.
#' @param vars_id character. Name of variables what you want to do the subsetting. By DEFAULT it make the subset for
#' all variables donwload on the folder create by \code{link{download_TERRACLIMATE}}.
#' @param dsn character. Path where the output folders and files will be saved, Default: getwd()
#' @param shapefile SpatialPolygonDataFrame. Shapefile to make the clip, it is obligatory to introduce, Default: NULL
#' @param years_sub integer. Years from which you want to do the subset, by DEFAULT it does to all the
#' available years of TERRACLIMATE download data by \code{link{download_TERRACLIMATE}}., Default: 1958:1960
#' @param factor numeric. Scale factor that will be multiplied to the netcdf values, Default: 0.1
#' @param aggr_mean_yearly logical. TRUE if you want to return the multianual aggregate raster of the variable(s). If FALSE
#' return the raster brick of the subsetting, Default: FALSE.
#' @param export_tif_stack logical. TRUE if you want to export the raster stack of the subsetting, if FALSE it will return
#' just the raster brick, Default: TRUE.
#' @param Rcolorbrewer_pal character. Name of Rcolorbrewer palet what yout want to do the plot, Default: 'Greens'
#' @return Subsettings exported raster tifs.
#' @references
#' Abatzoglou, J.T., S.Z. Dobrowski, S.A. Parks, K.C. Hegewisch, 2018, Terraclimate,
#' a high-resolution global dataset of monthly climate and climatic water balance from 1958-2015, Scientific Data,
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#' SHP <- rgdal::readOGR('Data/SHPs/subs.shp')
#' rst <- subsseting_TERRACLIMATE(vars_id = 'ppt',
#'                               shapefile = SHP,
#'                               years_sub = 2000:2002,
#'                               factor = 0.1,
#'                               aggr_mean_yearly = FALSE,
#'                               export_tif_stack = TRUE,
#'                               Rcolorbrewer_pal = 'Blues')
#'     }
#' }
#' @export
#' @rdname subsseting_TERRACLIMATE
#' @family subsetting functions
#' @author Crhistian Cornejo
#'
#' @importFrom tictoc tic toc
#' @importFrom sp proj4string CRS
#' @importFrom terra vect rast crop mask
#' @importFrom fs dir_ls
#' @importFrom purrr map
#' @importFrom raster projectRaster writeRaster stackApply rasterToPoints
#' @importFrom glue glue
#' @importFrom xfun dir_exists dir_create
#' @importFrom ggplot2 ggplot theme_void geom_tile aes scale_fill_gradientn geom_sf theme unit element_text labs ggsave
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sf st_as_sf
#' @importFrom plotly ggplotly



subsseting_TERRACLIMATE <- function(

  vars_id  = c('aet','def','swe','q','soil','PDSI', 'pet','ppt',
               'srad','tmax','tmin','vap','vpd','ws'),
  dsn = getwd(),
  shapefile = NULL,
  years_sub = 1958:1960,
  factor = 0.1,
  aggr_mean_yearly = FALSE,
  export_tif_stack = TRUE,
  Rcolorbrewer_pal = 'Greens'
){

  options(scipen = 1000, warn = -1)
  tictoc::tic()
  require(dplyr)
  ### error's messages
  if (is.null(shapefile)) {
    stop('For subsetting TERRACLIMATE grid data you need a shapefile object', call. = FALSE)
  }
  if (!vars_id %in% c('aet','def','swe','q','soil','PDSI', 'pet','ppt','srad','tmax','tmin','vap','vpd','ws') ) {
    stop('The variable ID does not exist in TERRACLIMATE data sets', call. = FALSE)
  }

  ## loading variables
  sp::proj4string(shapefile) <- sp::CRS("+init=epsg:4326")
  shape <- shapefile
  shapefile <- terra::vect(shapefile)

  if(!class(shapefile) == 'SpatVector') stop('subs shuold be a SpatVector  object')

  ls <- fs::dir_ls(paste0(dsn,'/download_TERRACLIMATE/Individual_years/'),regexp = '.*nc')
  lst_test <- base::sort(base::grep(vars_id, ls, value = T))

  #handling dir_ls for subsetting
  rstr <- purrr::map(.x = 1:length(years_sub), .f = function(i){

    cat('\f')
    message('creating subsetting raster object for year ',years_sub[i])
    var <- base::grep(years_sub[i], lst_test, value = T) %>%
      as.character() %>%
      terra::rast(lst_test) %>%
      terra::crop(.,shapefile) %>%
      terra::mask(., shapefile)

    return(var)
  })

  #selecting returning raster
  stack_rast <- terra::rast(rstr)*factor
  raster <- as(stack_rast, "Raster")
  raster::projectRaster(raster,crs = "+init=epsg:4326")

  if (export_tif_stack == TRUE) {
    out <- glue::glue('{dsn}/download_TERRACLIMATE/Individual_years/Subsetting_rasters/')
    ifelse(!xfun::dir_exists(out), xfun::dir_create(out, recursive = TRUE), print('Folder exists'))
    raster::writeRaster(raster, filename = paste0(out,vars_id,'_',head(years_sub,1),'_to_',
                                                  tail(years_sub, 1) ,'_stack.tif'),
                        overwrite=TRUE)

    cat('\f')
    message('Subsetting raster exported!!')
    return(raster)

  }else{

    cat('\f')
    message('Finish process!')

    return(raster)}

  if (aggr_mean_yearly == TRUE) {


    mes <- seq(as.Date(paste0(head(years_sub,1),'-01-01')),as.Date(paste0(tail(years_sub,1),'-01-01')),by='month')
    ind <- as.numeric(format(mes,'%Y'))
    r_yr  <- raster::stackApply(raster, indices=ind, fun='sum')
    r_yr_mean <-  raster::stackApply(r_yr, indices=rep(1,35), fun='mean')

    names(r_yr_mean) <- paste0('average_multiannual_',vars_id)

    rdf <- r_yr_mean %>% raster::rasterToPoints() %>%
      as.data.frame() %>%
      `colnames<-`(c("x", "y", names(r_yr_mean)))

    g <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_tile(
        data = rdf,
        ggplot2::aes(
          x = x,
          y = y,
          fill = rdf[,3])) +
      ggplot2::scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, Rcolorbrewer_pal))+
      ggplot2::geom_sf(data=sf::st_as_sf(shape),
                       colour='darkred',
                       linetype='solid',
                       fill = NA,
                       size=0.2)+
      ggplot2::theme(
        legend.key.width = ggplot2::unit(3, 'line'),
        legend.key.height = ggplot2::unit(1, 'line'),
        legend.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 10),
        legend.position = 'bottom')+
      ggplot2::labs(
        x = 'Lontigute', y = 'Latitude',
        fill = paste0('average\nmultiannual\n',vars_id),
        caption = "Plot generated by Package: RWeatherTools")

    ggplot2::ggsave(plot = g, filename = paste0(dsn,'/download_TERRACLIMATE/Individual_years/Subsetting_rasters/AverageAnual_plot.png'),
                    units = 'in',height = 8, width = 6, dpi = 900)

    # suppressWarnings(print(g))
    suppressWarnings(print(plotly::ggplotly(g)))
    cat('\f')
    message('multianual average raster successfully, exported and plotting interactive visualization!!!')

    return(raster)

  }else{
    cat('\f')
    message('Finish process!')
    return(raster)

  }

  tictoc::toc()
}



