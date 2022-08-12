################################################################################
#### FUNCTIONS TO GET AND HANDLING DATA FROM SENAMHI WATER STATIONS (M, H)
################################################################################
#' @title Get information from multiple station and variables from Servicio Nacional de Meteorología e Hidrológia del Perú (SENAMHI).
#' @param cod character. Codes of the station you want to download, to see the codes use \code{link{get_codes}}. For the moment just for Conventional stations data are available.
#' @param start_date character. Start date to filter the data what you want to download. Please write as \code{'yyyy-mm-dd'}.
#' @param end_date character. End date to filter the data what you want to download. Please write as \code{'yyyy-mm-dd'}.
#' @param convert2monthly logical.If \code{TRUE} will return the data in monthly values.
#' @param convert2daily logical. If \code{TRUE} will return the data in daily values (this just works for precipitation data).
#' @param na.rm logical. \code{TRUE} for remove NA's values from data if \code{convert2monthly} is \code{TRUE}. \code{FALSE} will return the monthly mean values convert2monthly NA's.
#' @param vars character. Variables to found in the database from SEAMHI and return.
#' @return
#'
#' A matrix with each station data of variable request, values of precipitation on (mm/hour, mm/day), temperature values on (°C) and
#' wind data on direction and velocity unit (m/s)
#'
#' @family SENAMHI
#' @importFrom senamhiR senamhiR
#' @importFrom dplyr select filter rename mutate  %>%
#' @importFrom tidyr complete pivot_wider
#' @importFrom hydroTSM daily2monthly.data.frame
#' @importFrom tibble rownames_to_column as_tibble
#'
#' @author Crhistian Cornejo
#'
#' @references this functions make usefull easy wrangling download data from the package senamhiR from
#' source("https://gitlab.com/snippets/1793256/raw");install("senamhiR")
#'
#' @examples
#' \dontrun{
#' #example for get the codes and pre-process for get_SENAMHI_data function
#' names <- c('Asuncion','Augusto','llica',
#' 'honda','llagaden','namora',
#' 'jesus','la victoria','celendin')
#' codes <- get_codes(stations_names = names,
#'                   type = 'Meteorological',
#'                   get_LongLat = TRUE,
#'                   plotStations = TRUE)
#'
#'  codes_for_function <- do.call(rbind, lapply(codes[1], data.frame,
#'  stringsAsFactors=FALSE)) %>%
#'  dplyr::arrange(StationID) %>%
#'  dplyr::select(StationID) %>%
#'  purrr::as_vector()

#' #for temperature
#' Data_T <- get_SENAMHI_data(cod = codes_for_function,
#'                           start_date = '2000-01-01',
#'                           end_date = '2015-12-31',
#'                           convert2monthly = FALSE,
#'                           convert2daily = FALSE,
#'                           na.rm = FALSE,
#'                           vars = 'tmp')

#' #getting the names matching names as vector for replace on the export
#' real_names <- do.call(rbind, lapply(codes[1], data.frame, stringsAsFactors=FALSE)) %>%
#'  dplyr::filter(StationID %in%
#'  codes_for_function) %>%
#'  dplyr::arrange(StationID) %>%
#'  dplyr::select(1) %>% as.data.frame() %>% purrr::as_vector(.)
#'
#' nombres_est <- as.vector(real_names)
#' colnames(Data_T) <- c('Date',rep(paste0('tmax_',nombres_est),1),
#' rep(paste0('tmin_',nombres_est),1))

#' #for precipitation
#' Data_P <- get_SENAMHI_data(cod = codes_for_function,
#'                           start_date = '2000-01-01',
#'                           end_date = '2015-12-31',
#'                           convert2monthly = FALSE,
#'                           convert2daily = FALSE,
#'                           na.rm = FALSE,
#'                           vars = 'pr')
#' #getting the names matching names as vector for replace on the export of precipitation dataframe
#'real_names_p <- do.call(rbind, lapply(codes[1], data.frame, stringsAsFactors=FALSE)) %>%
#'  dplyr::filter(StationID %in% codes_for_function) %>%
#'  dplyr::arrange(StationID) %>% dplyr::select(1) %>% as.data.frame() %>% purrr::as_vector(.)
#'
#' nombres_est_p <- as.vector(real_names_p)
#' colnames(Data_P) <- c('Date',rep(paste0('Prec07_',nombres_est),1),
#'                              rep(paste0('Prec19_',nombres_est),1))
#' write.csv(Data_P, '../SENAMHI_Prec_2000_2015_subdaily.csv')
#' }
#'
#' @export
#'


get_SENAMHI_data <- function(
    cod = NULL,
    start_date = NULL,
    end_date = NULL,
    convert2monthly= FALSE,
    convert2daily = FALSE,
    na.rm = T,
    vars = c('pr','tmp','wind')) {

  ## get data from senamhiR package

  if (vars == 'tmp') {

    data <- senamhiR::senamhiR(cod, collapse = T) %>%
      dplyr::select(1:4) %>%
      dplyr::filter(Fecha >= start_date & Fecha <= end_date) %>%
      dplyr::rename(Tmax='Tmax (C)',Tmin = 'Tmin (C)') %>%
      tidyr::complete(Fecha = seq.Date(min(Fecha),max(Fecha), by="day"))

    data2 <- data %>% tidyr::pivot_wider(data = .,names_from = StationID,values_from = c(Tmax, Tmin),names_sort = T)


    ## 2montlhy
    if(convert2monthly == TRUE){

      data2 <- as.data.frame(data2)
      data3 <- hydroTSM::daily2monthly.data.frame(x = data2,FUN = mean,na.rm = na.rm,
                                                  dates =1,date.fmt = '%Y-%m-%d',
                                                  out.type = 'data.frame' )

      mensual <- tibble::rownames_to_column(as.data.frame(data3), var = 'Fecha') %>%
        dplyr::mutate(Fecha = seq.Date(from = as.Date(start_date),to = as.Date(end_date),by = 'month'))

      cat('\f')
      message('----- conversion to monthly successful! -----\n')
      return(tibble::as_tibble(mensual))

    }else{
      cat('\f')
      message('Returning daily maximum and minimum temperature values for each water station\n')
      return(tibble::as_tibble(data2))

    }

  }

  if (vars == 'pr') {

    pr <- senamhiR::senamhiR(cod, collapse = T) %>%
      dplyr::select(1:2,11:12) %>%
      dplyr::filter(Fecha >= start_date & Fecha <= end_date) %>%
      dplyr::rename(Prec07='Prec07 (mm)',Prec19 = 'Prec19 (mm)') %>%
      tidyr::complete(Fecha = seq.Date(min(Fecha),max(Fecha), by="day"))

    pr2 <- pr %>% tidyr::pivot_wider(data = .,names_from = StationID,values_from = c(Prec07 , Prec19),names_sort = T)


    ## 2montlhy
    if(convert2daily == TRUE){

      stop('Daily convertion is not implemented yet!')

    }else{
      cat('\f')
      message('Returning 12 hourly precipitation values for each water station\n')
      return(tibble::as_tibble(pr2))

    }

  }

  if (vars == 'wind') {

    wd <- senamhiR::senamhiR(cod, collapse = T) %>%
      dplyr::select(1:2,13:14) %>%
      dplyr::filter(Fecha >= start_date & Fecha <= end_date) %>%
      dplyr::rename(wind_dir='Direccion del Viento',wind_vel = 'Velocidad del Viento (m/s)') %>%
      tidyr::complete(Fecha = seq.Date(min(Fecha),max(Fecha), by="day"))

    wd2 <- wd %>% tidyr::pivot_wider(data = .,names_from = StationID,values_from = c(wind_dir , wind_vel),names_sort = T)


    ## 2montlhy
    if(convert2monthly == TRUE){

      stop("Sorry for wind variable monthly convertion aren't available", call. = FALSE)

    }

  }

}


#######################################
## get codes from senamhi stations
#######################################
#' @title  Function to get codes of senamhi water stations.
#' @param stations_names character. Names of the stations we want to download and get their code.
#' @param type character. Type of stations from which the codes will be returned.
#' @param get_LongLat logical. If \code{TRUE} Longitude and Latitude will added to the output dataframe. This needed for \code{plotStations} function.
#' @param plotStations logical. \code{TRUE} will plot a graph of ubications of the stations and his altitude.
#' @family SENAMHI
#' @return
#' If \code{get_LongLat} and \code{plotStations} are TRUE a list with the table and a plot of the stations is returned,
#' if both are FALSE, only a two-column dataframe will be returned.
#'
#' @importFrom senamhiR station_search
#' @importFrom dplyr filter select %>%
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal
#' @importFrom plotly ggplotly
#'
#' @author Crhistian Cornejo
#'
#' @references this functions make usefull easy wrangling download data from the package senamhiR from
#' source("https://gitlab.com/snippets/1793256/raw");install("senamhiR")
#' @examples  \dontrun{
#' #example for get the codes and pre-process for get_SENAMHI_data function if return is a list
#' names <- c('Asuncion','Augusto','llica','honda','llagaden',
#'            'namora','jesus','la victoria','celendin')
#' codes <- get_codes(stations_names = names,
#'                   type = 'Met',
#'                   get_LongLat = TRUE,
#'                   plotStations = TRUE)
#'
#'  codes_for_function <- do.call(rbind, lapply(codes[1], data.frame, stringsAsFactors=FALSE)) %>%
#'  dplyr::arrange(StationID) %>%
#'  dplyr::select(StationID) %>%
#'  purrr::as_vector()
#'
#'
#' }
#'
#' @export

get_codes <- function(
    stations_names = NULL,
    type = c('Met','Hid'),
    get_LongLat = TRUE,
    plotStations = TRUE){

  # errors messages

  if (is.null(stations_names)) {
    stop('Please enter names of stations of your interest to find the codes', call. = FALSE)
  }

  if (get_LongLat == FALSE & plotStations == TRUE) {
    stop("LongLat needed!, if you want to get the plot, get_LongLat must be 'TRUE'", call. = FALSE)
  }

  if (type == 'Met') {

    if (get_LongLat == TRUE & plotStations == TRUE) {

      names_st <- senamhiR::station_search(stations_names)
      names_st_filter <- names_st %>% dplyr::filter(Type == 'CON' & Configuration == 'M') %>%
        dplyr::select(Station,StationID,Longitude,Latitude, Altitude) %>% dplyr::filter(StationID < 150000)

      g <- names_st_filter %>%
        ggplot2::ggplot(ggplot2::aes(Longitude,Latitude, size = Altitude, color = Station))+
        ggplot2::geom_point()+
        ggplot2::theme_minimal()

      g <- plotly::ggplotly(g)

      cat('\f')
      message(paste('----- Only',length(names_st_filter$StationID),'of',length(stations_names), 'names have been found! -----\n', sep = ' '))

      list <- list(Codes_table = names_st_filter, Plot = g)
      return(print(list))
    }else{

      names_st <- senamhiR::station_search(stations_names)
      names_st_filter <- names_st %>% dplyr::filter(Type == 'CON' & Configuration == 'M') %>%
        dplyr::select(Station,StationID) %>% dplyr::filter(StationID < 150000)
      cat('\f')
      message(paste('----- Only',length(names_st_filter$StationID),'of',length(stations_names), 'names have been found! -----\n', sep = ' '))
      return(print(names_st_filter))

    }

  }

  if (type == 'Hid') {

    if (get_LongLat == TRUE & plotStations == TRUE) {
      names_st <- senamhiR::station_search(stations_names)
      names_st_filter <- names_st %>% dplyr::filter(Type == 'HLG' & Configuration == 'H') %>%
        dplyr::select(Station,StationID,Longitude,Latitude, Altitude)

      #if plot is true

      g <- names_st_filter %>%
        ggplot2::ggplot(ggplot2::aes(Longitude,Latitude, size = Altitude, color = Station))+
        ggplot2::geom_point()+
        ggplot2::theme_minimal()

      g <- plotly::ggplotly(g)

      cat('\f')
      message(paste('----- Only',length(names_st_filter$StationID),'of',length(stations_names), 'names have been found! -----\n', sep = ' '))

      list <- list(Codes_table = names_st_filter, Plot = g)
      return(print(list))
    }else{
      names_st <- senamhiR::station_search(stations_names)
      names_st_filter <- names_st %>% dplyr::filter(Type == 'HLG' & Configuration == 'H') %>%
        dplyr::select(Station,StationID)
      cat('\f')
      message(paste('----- Only',length(names_st_filter$StationID),'of',length(stations_names), 'names have been found! -----\n', sep = ' '))
      return(print(names_st_filter))

    }


  }


}


