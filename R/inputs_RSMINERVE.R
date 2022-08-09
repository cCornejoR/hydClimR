
#' @title Observerd gauge data to RS MINERVE database as input for modelling.
#' @description Converter you excel sheets data of gauge time series into data base .csv for reading
#' as input into RS Minerve.
#' @param path_file string path to excel file to be read, see \code{link{Template_sheet_MINERVE}} to see how to
#' order your observed data and the function is able to generate the csv output.
#' Please do not change the names of the sheets.
#' @param tz Time zone string to be passed to base::format. Default: 'UTC'.
#' @param units units of your observed data, Default: c("C", "mm/d", "m3/s").
#' @param save_path string path where the csv output will be saved, Default: getwd().
#' @param interpolation_method string name of the interpolation method to use for NA's values and PET in RS Minerve, Default: 'Linear'.
#' @param dates_rs list with the start and end date to generate the dates that RS minerve can read, Default: list(Start = "2007-01-01", end = "2010-12-31")
#' @return DataFrame ass DataBase for read in RSMinerve.
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


