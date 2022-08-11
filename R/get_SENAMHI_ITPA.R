#' @title Obtain information from SENAMHI on Advanced Thermo-Pluviometric information from existing stations, this information is available from 2011 to the present.
#' You can quickly generate an Exploratory Data Analysis for the entire requested data set.
#' @param year_filter character string year from where the data with information available for that particular year are granted
#' @param station character vector of stations names optained from \code{\link{stations_ITPA_SENAMHI}}
#' @param export_csv logical, If \code{TRUE} it will export the table as a .csv file on yout local home working directory.
#' @param fast_EDA logical. Compute a fast Exploratory Data Analysis of the information.
#' @return If fast_EDA is TRUE will return a list with several information form SENAMHI ITPA,
#' firts list: '\code{list[[1]]}' is a tibble with the information of stations request, second list: '\code{list[[2]]}',
#' is the results from fast Exploratory Data ANALYSIS from the variables in the tibble. Also it export a .csv file
#' to your local home working directory.
#' @family SENAMHI
#' @examples
#' \dontrun{
#' if(interactive()){
#'station <- c('AUGUSTO WEBERBAUER', 'NAMORA')
#'test <- get_ITPA_SENAMHI(year_filter = '2011',
#'                         station = station,
#'                         export_csv = TRUE,
#'                         fast_EDA = TRUE)
#'
#'tabla_senamhi <- as_tibble(test[[1]])
#'missing_values <- test[[3]]
#'  }
#' }
#' @rdname get_ITPA_SENAMHI
#' @export
#' @importFrom readr read_csv
#' @importFrom purrr map
#' @importFrom plyr ldply
#' @importFrom dplyr select mutate group_by filter ungroup arrange %>%
#' @importFrom tidyr pivot_longer complete pivot_wider
#' @importFrom SmartEDA ExpData ExpNumStat ExpNumViz ExpOutQQ
#' @importFrom tibble as_tibble
#' @importFrom DataExplorer plot_missing plot_boxplot
#' @importFrom ggplot2 theme_minimal
#'
get_ITPA_SENAMHI <- function(
    year_filter,
    station = NULL,
    export_csv = TRUE,
    fast_EDA = TRUE){

  options ( warn = - 1)
  # data getting from: 'https://www.senamhi.gob.pe/site/lvera/red_rpm2.php'

  base <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTq0UTKU1u7tNVK56sLs-zDUtWhNctDquBMT7eHAoTHmstdA6gQS_Lx31W4f0G9kw/pub?gid='
  final <- '&single=true&output=csv'
  sheets <- c(1698968598,2086715188,1568222612,
              74906467,1048876332,1998056984,
              1680761533,288030279,1981876818,
              1070844760,1489464130,311060303)

  df <- data.frame('2011' = sheets[[1]],
                   '2012' = sheets[[2]],
                   '2013' = sheets[[3]],
                   '2014' = sheets[[4]],
                   '2015' = sheets[[5]],
                   '2016' = sheets[[6]],
                   '2017' = sheets[[7]],
                   '2018' = sheets[[8]],
                   '2019' = sheets[[9]],
                   '2020' = sheets[[10]],
                   '2021' = sheets[[11]],
                   '2022' = sheets[[12]],
                   check.names = TRUE,
                   fix.empty.names = TRUE)

  url_list <- list()

  for (i in 1:length(df)) {
    url_list[[i]] <- paste0(base,df[i],final)
  }

  cat('\f')
  message('Reading data and returning data request from database...')
  data_csv =  plyr::ldply(url_list, function(...)readr::read_csv(...,col_types = list()), .progress = 'text') %>%
    dplyr::filter(.,ESTACION %in% station & YEAR %in% year_filter) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(ESTACION)

  cat('\f')

  df_export <- data_csv %>%
    dplyr::select(CODIGO,ESTACION, YEAR, MES, VAR,12:42 ) %>%
    tidyr::pivot_longer(6:36, names_to = 'DAYS', values_to = 'VAR_VALUES') %>%
    dplyr::mutate(DATES = as.Date(paste(YEAR,MES,DAYS,sep = '-'), format= '%Y-%m-%d'),
                  VAR_VALUES = as.numeric(VAR_VALUES)) %>%
    dplyr::group_by(DATES) %>%
    dplyr::filter(!any(is.na(DATES))) %>%
    dplyr::ungroup() %>%
    tidyr::complete(DATES = seq(min(DATES), max(DATES), by = "day"),
                    fill = list(VALOR = NA)) %>%
    dplyr::select(CODIGO,ESTACION,DATES,VAR,VAR_VALUES) %>%
    tidyr::pivot_wider(names_from = VAR, values_from = VAR_VALUES) %>%
    dplyr::arrange(CODIGO)

  df_export <- within(df_export, PP24[PP24 < 0] <- NA)
  df_export <- within(df_export, TMAX[TMAX = -999] <- NA)
  df_export <- within(df_export, TMIN[TMIN = -999] <- NA)
  df_export <- df_export %>% dplyr::mutate(TAVG = (TMAX+TMIN)/2)

  if (fast_EDA == TRUE) {


    EDA_1 <- SmartEDA::ExpData(data=df_export,type=2)
    EDA_1 <- EDA_1[-1:-3,c(-1,-3)]
    EDA_1 <- tibble::as_tibble(EDA_1)

    EDA_2 <- SmartEDA::ExpNumStat(df_export,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)
    EDA_2 <- tibble::as_tibble(EDA_2)

    f <- SmartEDA::ExpOutliers(df_export, varlist = c("PP24","TMAX","TMIN"), method = "3xStDev",  capping = c(0.1, 0.9), outflag = TRUE) %>%
      plyr::ldply(., tibble::as_tibble)

    plot_1 <- SmartEDA::ExpNumViz(df_export,target=NULL,type=1,nlim=10,col=NULL,Page=c(2,2))

    plot_2 <- SmartEDA::ExpOutQQ(df_export,
                                 nlim=4,
                                 fname=NULL,
                                 Page=c(2,2))

    plot_3 <- SmartEDA::ExpNumViz(df_export[,5:7],
                                  Page=c(3,1),
                                  scatter=TRUE)

    plot_4 <- DataExplorer::plot_missing(df_export[,4:7],
                                         title = 'Missins Values on SENAMHI DATA plot',
                                         ggtheme = ggplot2::theme_minimal())


    plot_5 <- suppressWarnings(DataExplorer::plot_boxplot(df_export,by = 'ESTACION',
                                                          geom_boxplot_args = list("outlier.color" = "red"),
                                                          ncol = 2L,ggtheme = ggplot2::theme_minimal()))

    plots_list <- list(
      MissingValuesplot = plot_4,
      BoxplotOutliers = plot_5,
      DensityPlot = plot_1,
      QQplot = plot_2,
      Scatterplot = plot_3
    )

    tabbles_stats <- list(
      Missins = EDA_1,
      stats = EDA_2
    )

    all_list <- list(tibble_data = df_export,
                     plots = plots_list,
                     Tabbles = tabbles_stats)
    print(all_list)

    cat('\f')
    message('Done!')
    return(all_list)

  }else{

    cat('\f')
    message('Done!')
    print(df_export)
    return(df_export)

  }

  if (export_csv == TRUE) {
    write.csv(x = df_export, file = paste0(glue::glue(getwd(),'/SENAMHI_ITPA/'),'data_request.csv'))
  }

}


####################################################################################################
## obtener estaciones que existen en el filtro de datos -------------------------------------------
####################################################################################################

#' @title Get the available stations filter by year(s) for use in the function \code{\link{get_ITPA_SENAMHI}}.
#' @param names vector. Vector names for use in the function to search matching with the information from the database.
#' @param years Interger year from where the data with information available for that particular year are granted.Default: 2011:2015
#' @param lonlat logical, if \code{TRUE} will returning a tibble with longitude and latitude, this is needed if you want
#' to plot it. Default: FALSE
#' @param plot_stations logical, \code{TRUE} will be plot on a Open Streat Map by leaflet the ubication of the stations matched. Default: FALSE
#' @return Names of the stations to use in \code{\link{get_ITPA_SENAMHI}}
#' @family SENAMHI
#' @examples
#' \dontrun{
#' if(interactive()){
#' names <- c('namora', 'celendin','el tigre','asuncion','papayal')
#'
#' t <- stations_ITPA_SENAMHI(names = names,
#'                           years = 2011:2012,
#'                           lonlat = TRUE,
#'                           plot_stations = TRUE)
#'  }
#' }
#' @rdname stations_ITPA_SENAMHI
#' @export
#' @importFrom plyr ldply
#' @importFrom readr read_csv
#' @importFrom dplyr filter arrange select mutate  %>%
#' @importFrom tibble as_tibble
#' @importFrom leaflet leaflet addTiles addMarkers
#' @importFrom leaflet.extras addResetMapButton addSearchFeatures searchFeaturesOptions

stations_ITPA_SENAMHI <- function(
    names=NULL,
    years = 2011:2015,
    lonlat = FALSE,
    plot_stations = FALSE
){

  if (!lonlat  & plot_stations == TRUE) {
    stop('"lonlat" argument must be TRUE for plot stations ubications', call. = FALSE)
  }

  if (!is.null(names)) {

    base <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTq0UTKU1u7tNVK56sLs-zDUtWhNctDquBMT7eHAoTHmstdA6gQS_Lx31W4f0G9kw/pub?gid='
    final <- '&single=true&output=csv'
    sheets <- c(1698968598,2086715188,1568222612,
                74906467,1048876332,1998056984,
                1680761533,288030279,1981876818,
                1070844760,1489464130,311060303)

    df <- data.frame('2011' = sheets[[1]],
                     '2012' = sheets[[2]],
                     '2013' = sheets[[3]],
                     '2014' = sheets[[4]],
                     '2015' = sheets[[5]],
                     '2016' = sheets[[6]],
                     '2017' = sheets[[7]],
                     '2018' = sheets[[8]],
                     '2019' = sheets[[9]],
                     '2020' = sheets[[10]],
                     '2021' = sheets[[11]],
                     '2022' = sheets[[12]],
                     check.names = TRUE,
                     fix.empty.names = TRUE)

    url_list <- list()

    for (i in 1:length(df)) {
      url_list[[i]] <- paste0(base,df[i],final)
    }

    names <- glob2rx(names)
    if (length(names) > 1) names <- paste(names, collapse = "|")
    cat('\f')
    message('Reading data and returning data request from database...')
    data_csv =  plyr::ldply(url_list, function(...)readr::read_csv(...,col_types = list()), .progress = 'text')

    filter_one <- data_csv %>%
      dplyr::filter(.,grepl(names, data_csv$ESTACION, ignore.case = TRUE)) %>%
      dplyr::filter(YEAR %in% as.character(years)) %>%
      tibble::as_tibble() %>%
      dplyr::arrange(ESTACION) %>%
      dplyr::select(CODIGO,ESTACION,LONSIG,LATSIG,ALT, YEAR) %>%
      unique()

    if (lonlat == TRUE & plot_stations == TRUE) {
      lon_lat <- filter_one %>%
        dplyr::select(CODIGO,ESTACION,LONSIG,LATSIG,ALT) %>%
        dplyr::mutate(LONSIG = as.numeric(LONSIG),
                      LATSIG = as.numeric(LATSIG))

      map <- lon_lat %>%
        leaflet::leaflet() %>% leaflet::addTiles() %>%
        leaflet::addMarkers(lng = ~LONSIG, lat = ~LATSIG,
                            label = ~ESTACION,
                            popup = paste(paste('<b>CODIGO:</b>',
                                                lon_lat$CODIGO),
                                          paste('<b>Longitude:</b>',
                                                lon_lat$LONSIG),
                                          paste('<b>Latitude:</b>',
                                                lon_lat$LATSIG),
                                          paste('<b>Altitude:</b>',
                                                lon_lat$ALT,'m'),
                                          sep = '<br/>'),
                            group = 'lon_lat') %>%
        leaflet.extras::addResetMapButton() %>%
        leaflet.extras::addSearchFeatures(
          targetGroups = 'lon_lat',
          options = leaflet.extras::searchFeaturesOptions(zoom = 15,
                                                          openPopup = TRUE,
                                                          firstTipSubmit = TRUE,
                                                          autoCollapse = TRUE,
                                                          hideMarkerOnCollapse = TRUE))

      print(map)

      cat('\f')
      cat('Only:',paste0(filter_one$ESTACION),'\nAre available for period years request from', head(years,1),'to',paste0(tail(years,1),'.'),
          '\nPlease use this names exactly of stations to get values on the function\n"get_ITPA_SENAMHI"')

      return(filter_one)

    }else{

      filter_two <- filter_one %>%
        dplyr::select(1,2,6)

      cat('\f')
      cat('Only:',paste0(filter_one$ESTACION),'\nAre available for period years request from', head(years,1),'to',paste0(tail(years,1),'.'),
          '\nPlease use this names exactly of stations to get values on the function\n"get_ITPA_SENAMHI"')
      message('Done!')
      return(filter_two)

    }

  } else{
    stop('Names of stations needed to get availability information from Database', call. = FALSE)
  }

}



