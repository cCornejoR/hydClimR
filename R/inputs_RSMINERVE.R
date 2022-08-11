
#' @title Converter you excel sheets data of gauge time series into data base .csv for reading
#' as input into RS Minerve platform for modelling.
#' @param path_file string path to excel file to be read, see \code{link{Template_sheet_MINERVE}} to see how to
#' order your observed data and the function is able to generate the csv output.
#' Please do not change the names of the sheets.
#' @param tz Time zone string to be passed to base::format. Default: 'UTC'.
#' @param units units of your observed data, Default: c("C", "mm/d", "m3/s").
#' @param save_path string path where the csv output will be saved, Default: getwd().
#' @param interpolation_method string name of the interpolation method to use for NA's values and PET in RS Minerve, Default: 'Linear'.
#' @param dates_rs list with the start and end date to generate the dates that RS minerve can read, Default: list(Start = "2007-01-01", end = "2010-12-31")
#' @return DataFrame ass DataBase for read in RSMinerve.
#' @family Inputs Hidrological models
#' @examples
#' \dontrun{
#' if(interactive()){
#' path_file <- 'data/Stations_MINERVE.xlsx'
#'
#' stations2RSMinerve(path_file = path_file)
#'
#'  }
#' }
#' @rdname stations2RSMinerve
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr arrange select %>%
#'
stations2RSMinerve <- function(
    path_file,
    tz = 'UTC',
    units = c('C', 'mm/d','m3/s'),
    save_path = getwd(),
    interpolation_method = 'Linear',
    dates_rs = list(Start = '2007-01-01', end = '2010-12-31')

  ){

  #hearders of stations
  headers <- readxl::read_xlsx(path_file, sheet = 'data')
  headers <- colnames(headers)
  remove  <- sprintf("...%s",seq(from = 3 , to = length(headers), by = 2))
  headers <- headers[!headers %in% remove]
  # names <- sapply(head_coords, function (x) rep(x,2)) %>% as.vector()


  #coords
  transpose_coords <- function(path_file){

    coords_xlsx <- readxl::read_xlsx(path_file, sheet = 'stations')
    co <- t(coords_xlsx) %>% as.data.frame()
    co <- tibble::rownames_to_column(co)

    cor <- co %>% dplyr::arrange(rowname)
    n <- length(cor)-1
    cors <- cbind(cor, rep(cor[,c(2:n)],1))
    data_coords <- cors[,c(colnames(cors)[colnames(cors)!= paste0('V',n)], paste0('V',n))]

  }

  coords_df <- transpose_coords(path_file)

  #data wrangling
  data_xlsx <- readxl::read_xlsx(path_file, skip = 1, sheet = 'data')

  cat('\f')
  Temp <- data_xlsx %>% dplyr::select(starts_with('T'))
  colnames(Temp) <- rep('T', length(Temp))

  Prec <- data_xlsx %>% dplyr::select(starts_with('P'))
  colnames(Prec) <- rep('P', length(Prec))

  Flow <- data_xlsx %>% dplyr::select(starts_with('Q'))
  colnames(Flow) <- rep('Q', length(Flow))


  #times
  dates <- seq.Date(as.Date(dates_rs[[1]], format = '%Y-%m-%d'),
                    as.Date(dates_rs[[2]], format = '%Y-%m-%d'),
                    by = 'day')

  dates  <- gsub('-','.',as.character(dates))
  years  <- substr(dates,1,4)
  months <- substr(dates,6,7)
  days   <- substr(dates,9,10)
  dates_rev <- paste(days,months,years, sep = '.')
  dates_final  <- paste(dates_rev, rep('00:00:00', length(dates)), sep = ' ')

  #sensors
  n_l <- length(headers)-2
  sensor_T <- rep('T',n_l)
  sensor_P <- rep('P',n_l)
  sensor_Q <- 'Q'
  Sensor <- c(sensor_T,sensor_P,sensor_Q)

  #cats
  cat_T <- rep('Temperature',n_l)
  cat_P <- rep('Precipitation',n_l)
  cat_Q <- 'Flow'
  Category <- c(cat_T,cat_P,cat_Q)

  #units
  unit_T <- rep(units[1],n_l)
  unit_P <- rep(units[2],n_l)
  unit_Q <- units[3]
  Unit <- c(unit_T, unit_P, unit_Q)

  #interpolation
  interpolation_c <- rep(interpolation_method, 2*n_l)
  interpolation_q <- 'Constant before'
  Interpolation <- c(interpolation_c, interpolation_q)

  ##final
  utils_df <- data.frame(Sensor,Category,Unit,Interpolation)
  utils_df <- t(utils_df)
  utils_df <- as.data.frame(utils_df)
  utils_df <- tibble::rownames_to_column(utils_df)

  utils_df  <- data.frame(id = rownames(utils_df), utils_df)
  df_vars   <- data.frame(id = rownames(Temp),dates_final,Temp,Prec,Flow)
  coords_df <- data.frame(id = rownames(coords_df),coords_df)

  colnames(utils_df) <- colnames(coords_df)
  colnames(df_vars)  <- colnames(coords_df)
  data_list  <- list(coords_df,utils_df,df_vars)
  data_final <- do.call(rbind, data_list)
  data_final <- data_final[,-1]
  colnames(data_final) <- NULL

  write.csv(data_final, file = paste0(save_path,'/RSMINERVE_DB.csv'),quote = FALSE,row.names = FALSE)

  cat('\f')
  message('Done! now just import the .csv file into the RSMinerve database, good luck!')


}

################################################################################################################


#' @title Function extracts grid data from climate raster bricks
#' and make a database table for later import in RSMinerve platform.
#' @param grid raster brick or raster stack
#' @param subbasins PARAM_DESCRIPTION
#' @param var_type Either 'Temperature' or 'Precipitation'.Default: 'Precipitation'
#' @param crs 4 digit crs code to ensure projection consistency between raster and shapefile. Default: 32717
#' @param DEM Digitial Elevation Model (raster) for extracting z values.
#' @param dsn path directory to which you want to store the downloaded data.
#' @param dates_rs list with star_date and end_date of where the raster brick have information, Default: list()
#' @param interpolation_method type of interpolation reading for RSMinerve, Default: 'Linear'
#' @param cores numeric. Number of cores to be used for the download. By default,
#' it automatically detects how many cores there are on your computer and
#' subtracts 1 to prevent it from slowing down.
#' @family Inputs Hidrological models
#' @examples
#' \dontrun{
#' if(interactive()){
#' DEM <- raster::raster('usefull_data/pruebas/DEM_alos_JT.tif')
#' subbasins <- sf::st_read('usefull_data/pruebas/Subcuencas_v006.shp')
#' grid_pr <- raster::brick('usefull_data/pruebas/prec_diaria.nc')
#'
#' grid2RSMinerve(grid = grid_pr,
#'                subbasins = subbasins,
#'                var_type = 'Precipitation',
#'                crs = 32717,DEM = DEM,
#'                dsn = getwd(),
#'                interpolation_method = 'Linear',
#'                dates_rs = list(Start = '1981-01-01', end = '2016-12-31'))
#'  }
#' }
#' @rdname grid2RSMinerve
#' @export
#' @importFrom parallel detectCores makeCluster clusterEvalQ clusterExport parLapply stopCluster
#' @importFrom tictoc tic toc tic.log tic.clearlog
#' @importFrom sp proj4string CRS coordinates
#' @importFrom sf st_transform st_crs st_as_sf st_centroid st_coordinates
#' @importFrom raster crop extent raster crs res nlayers resample extract
#' @importFrom exactextractr exact_extract
#' @importFrom tibble as_tibble tibble add_column
#' @importFrom dplyr rename mutate_all %>%
#' @importFrom terra rast crs
#'
grid2RSMinerve <- function(
    grid,
    subbasins,
    var_type = 'Precipitation',
    crs = 32717,
    DEM,
    dsn = getwd(),
    dates_rs = list(),
    interpolation_method = 'Linear',
    cores = parallel::detectCores()-1

){

  options(warn = -1)
  tictoc::tic('timer')
  ### mathcing a crs of al inputs variables for extraction
  sp::proj4string(grid) <- sp::CRS("+init=epsg:4326")
  sf::st_transform(subbasins,crs = sf::st_crs(4326))
  cat('\f')


  names_subs <- subbasins$Subbasin

  #tavg
  cat('\f')
  cat('-------------------------------------------------------------------------------------\n')
  cat('performing extraction and generating database on:',cores,' cores, this may take some time!\n')
  cat('-------------------------------------------------------------------------------------\n')
  extract_fast <- function(brick,
                           shp,
                           resolution=0.01,
                           buffer=1.1){

    shp          <- sf::st_as_sf(shp)
    ras_buf      <- raster::crop(brick, raster::extent(shp)*buffer)
    ras_res      <- raster::raster(raster::extent(ras_buf[[1]]))
    raster::crs(ras_res) <- raster::crs(ras_buf)
    raster::res(ras_res) <- resolution
    cl <- parallel::makeCluster(cores)
    parallel::clusterEvalQ(cl,c(library(exactextractr), library(raster)))
    parallel::clusterExport(cl, varlist=c("ras_buf","ras_res","shp"), envir=environment())
    ras_mean <- parallel::parLapply(cl, 1:raster::nlayers(ras_buf), function(z) {
      res <- raster::resample(ras_buf[[z]], ras_res, method='ngb')
      ans <- as.numeric(exactextractr::exact_extract(res, shp, fun='mean'))
      return(ans)
    })
    parallel::stopCluster(cl)
    ras_sub <- round(do.call(rbind, ras_mean),1)
    return(ras_sub)

  }

  subbasin_data <- extract_fast(brick = grid,shp = subbasins)
  subbasin_df <- subbasin_data %>% tibble::as_tibble()

  #times
  dates <- seq.Date(as.Date(dates_rs[[1]], format = '%Y-%m-%d'),
                    as.Date(dates_rs[[2]], format = '%Y-%m-%d'),
                    by = 'day')

  dates  <- gsub('-','.',as.character(dates))
  years  <- substr(dates,1,4)
  months <- substr(dates,6,7)
  days   <- substr(dates,9,10)
  dates_rev <- paste(days,months,years, sep = '.')
  dates_final  <- paste(dates_rev, rep('00:00:00', length(dates)), sep = ' ') %>%
    tibble::as_tibble() %>% dplyr::rename(Station = value)

  datagrid_df <- cbind(dates_final,subbasin_df) %>% tibble::as_tibble()


  ### matching crs for extract x,y,z from dem on hrus
  xy_utm <-sf::st_transform(subbasins,crs = sf::st_crs(crs)) %>%
    sf::st_centroid(.) %>%
    sf::st_coordinates(.) %>%
    tibble::as_tibble()

  DEM <- terra::rast(DEM)
  terra::crs(DEM) <- paste0("epsg:",crs)
  DEM <- raster::raster(DEM)

  suppressWarnings(
  z <- raster::extract(DEM, sp::coordinates(xy_utm), fun = mean, na.rm = TRUE) %>%
    tibble::as_tibble() %>% dplyr::rename(Z = value))

  suppressWarnings(
  xyz <- cbind(xy_utm, z) %>% as.matrix() %>%
    t() %>% tibble::as_tibble() %>% dplyr::mutate_all(as.character))

  names(xyz) <- names_subs

  # Construct csv-file
  if (var_type == 'Precipitation') {
    Sensor <- rep('P',length(xyz))
    Unit <- rep('mm/d',length(xyz))
  }else{
    Sensor <- rep('T', length(xyz))
    Unit <- rep('C',length(xyz))
  }

  Category <- rep(var_type,length(xyz))
  Interpolation <- rep(interpolation_method,length(xyz))

  df_us <- rbind(Sensor,Unit,Category,Interpolation) %>% tibble::as_tibble()
  names(df_us) <- names(xyz)

  data_final_sns <- rbind(xyz,df_us) %>% tibble::as_tibble()
  tibble_add <- tibble::tibble(Station = c('X','Y','Z','Sensor','Unit','Category','Interpolation'))
  data_final_sns <- data_final_sns %>% tibble::add_column(tibble_add) %>%
    relocate(1:length(xyz), .after = last_col())

  names(datagrid_df) <- c('Station', names_subs)

  final_df <- rbind(data_final_sns,datagrid_df)

  tictoc::toc(log = TRUE, quiet = TRUE)
  log.txt <- tictoc::tic.log(format = FALSE)
  tictoc::tic.clearlog()
  timings <- unlist(lapply(log.txt, function(x) x$toc - x$tic))
  cat('\f')
  cat('----------------------------------------------------------------------\n')
  message('Done, time elapsed for pcp extraction:', round(timings/60,2),'minutes\n')
  cat('----------------------------------------------------------------------\n')
  write.csv(final_df,file = paste0(dsn,'/',var_type,'DB_RS.csv'),quote = FALSE,row.names = FALSE)






}


