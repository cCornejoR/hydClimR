################################################################################
#### Climatology
################################################################################
#' @title Compute climatology of grid data for a extension area of a shapefile basin.
#'
#' @description  Compute climatology of grid data for a extension area of a shapefile basin,
#' with the possibility of creating a graph of the climatology and exporting a GIF.
#' @param start_date character. Initial date from which the calculation of the climatology will be made,
#' this will be introduce as 'yyyy-mm-dd'.
#' @param end_date character. Final date from which the calculation of the climatology will be made,
#' this will be introduce as 'yyyy-mm-dd'.
#' @param years_omit numeric. Year or Years to be omited in the climatology, this will be usefull if you data have
#' extreme years.
#' @param grid_path Character. Path where the raster stack or netcdf is stored.
#' @param name_out Character. Output name to save graphics files, gifs and raster stack tif format.
#' @param dsn chracter. Path where the output folders and files will be created.
#' @param shapefile SpatialPolygon. Shapefile to make the clip if \code{return_clip} is TRUE.
#' @param main_title.plot character. Title for plots.
#' @param variable character. Name of variable what you are compute climatology.
#' @param save_GIF logical. If TRUE returns a gif of the climatology calculate,
#' the output of the extension will depend on the argument \code{return_clip}. if FALSE will only create a graph for visualization.
#' @param save_plot logical. If TRUE returns a plot of the climatology calculate,
#' the output of the extension will depend on the argument \code{return_clip}. if FALSE will only create a graph for visualization.
#' @param return_clip logical. if this argument is TRUE and \code{shapefile} is not NULL,
#' all climatology outputs will be computed and returned just for shapefile extension. If FALSE
#' return the climatology for the original grid data.
#' @return grid montlhy mean climatology of grid input.
#' @author Crhistian Cornejo
#' @examples
#' \dontrun{
#' if(interactive()){
#' shp <- rgdal::readOGR('Data/SHPs/cuenca_mayo.shp')
#' t <- climatology(years_omit = c(2005,2010),
#'                 grid_path = 'Data/GRIDs/prec_diaria.nc',
#'                 main_title.plot = 'Mayo River basin from PISCOpd',
#'                 shapefile = shp,
#'                 star_date = '1981-01-01',
#'                 end_date = '2016-12-01',
#'                 name_out = 'mayo_basin_climatology',
#'                 variable = 'Precipitation (mm)',
#'                 save_GIF = TRUE,
#'                 return_clip = TRUE,
#'                 save_plot = FALSE)
#'  }
#' }
#' @rdname climatology
#' @export
#' @import glue
#' @import xfun
#' @import raster
#' @import sp
#' @import dplyr
#' @import stringr
#' @import terra
#' @import tidyr
#' @import ggplot2
#' @import RColorBrewer
#' @import sf
#' @import gganimate
climatology <- function(

  start_date = '1981-01-01',
  end_date = '2016-12-01',
  years_omit = NULL,
  grid_path = NULL,
  name_out = NULL,
  dsn = getwd(),
  shapefile = NULL,
  main_title.plot = 'Climatology',
  variable = 'Precipitation\n(mm)',
  save_GIF = TRUE,
  save_plot = TRUE,
  return_clip = TRUE

){


  if (return_clip == TRUE & is.null(shapefile)) {
    stop('For return clipping area you need a shapefile object!!')
  }

  options(scipen = 1000,warn = -1)
  outp <- glue::glue(dsn,'/Climatology/')
  ifelse(!dir.exists(outp),xfun::dir_create(outp),print('already exist!'))

  cat('\f')

  if (!is.null(shapefile)) {

    cat('\f')
    brick_rst <- raster::brick(grid_path)
    sp::proj4string(brick_rst) <- sp::CRS("+init=epsg:4326")
    shapefile <- sp::spTransform(shapefile, raster::crs(brick_rst))

    if(sp::is.projected(shapefile)){
      stop('Error: Subbasins dont have projection')
    }  else {
      shapefile <- sp::spTransform(shapefile, raster::projection(brick_rst))

    }

    df <- tibble(
      date = seq(as.Date(star_date), as.Date(end_date), by = "1 month")
    ) %>%
      mutate(id = 1:n())
    fun.clim()
    fun.clim <- function(month, years.omit, data) {
      grd.mt <- df %>%
        dplyr::filter(
          stringr::str_sub(date, 6, 7) == month &
            stringr::str_sub(date, 1, 4) %nin% years.omit
        )

      data[[grd.mt$id]] %>%
        "*"(1) %>%
        mean(na.rm = T) %>%
        return()


    }

    grd.clim <- sapply(
      sprintf("%02d", 1:12),
      FUN = fun.clim,
      years.omit = years_omit,
      data = raster::brick(grid_path)
    ) %>%
      raster::stack() %>%
      "*"(1)
    crop <- terra::crop(grd.clim, shapefile)
    names(crop) <- month.name

    g_df <- crop %>%

      raster::rasterToPoints() %>%
      as.data.frame() %>%
      `colnames<-`(c("x", "y", names(crop))) %>%
      tidyr::pivot_longer(cols = 3:14,
                          names_to = "Months",
                          values_to = "val")

    myFun <- function(x, dummyDay = "", dummyYear = "2000"){
      require(lubridate)

      x <- ifelse(substr(x, 1, 3) %in% month.abb,
                  paste(match(substr(x, 1, 3), month.abb),
                        dummyDay,
                        dummyYear, sep = "/"), x)
      mdy(x)
    }

    g_df <- g_df %>%
      mutate(Date = myFun(Months),
             Months_fct = factor(Months, levels = c('January', 'February', 'March', 'April', 'May',
                                                    'June','July','August', 'September', 'October', 'November', 'December')))

    if (save_GIF == TRUE) {

      cat('\f')
      message('Computing Climatology and exporting GIF...')

      p <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::geom_tile(
          data = g_df,
          ggplot2::aes(
            x = x,
            y = y,
            fill = val,
            group = Months)) +
        ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'RdYlBu')))+
        ggplot2::geom_sf(data=sf::st_as_sf(shapefile),
                         colour='black',
                         linetype='solid',
                         fill = NA,
                         size=0.2)+
        ggplot2::theme(
          legend.key.width = ggplot2::unit(9, 'line'),
          legend.key.height = ggplot2::unit(2.5, 'line'),
          legend.text = ggplot2::element_text(size = 20),
          legend.title = ggplot2::element_text(size = 25),

          plot.title = ggplot2::element_text(
            family = "Prata",
            size = 40,
            hjust = 0.5),
          plot.subtitle = ggplot2::element_text(
            hjust = 0,
            size = 12),
          plot.caption = ggplot2::element_text(
            color = "grey40",
            size = 20,
            hjust = 1),
          legend.position = "bottom"
        ) +
        ggplot2::labs(
          x = 'Lontigute', y = 'Latitude',
          title = paste0("Climatology of " ,main_title.plot,"\nMonth: {closest_state}"),
          fill = variable,
          caption = "Plot generated by Package: RWeatherTools")+
        gganimate::transition_states(states = Months_fct)+
        gganimate::shadow_wake(wake_length = 0.5)

      gganimate::animate(
        p,
        width = 1200,
        height = 1200,
        renderer= gganimate::gifski_renderer(paste0(outp,name_out,'.gif'))
      )


    }else{

      cat('\f')
      message('Computing Climatology and plotting graph...')
      p2 <- ggplot2::ggplot() +
        ggplot2::theme_dark() +
        ggplot2::geom_tile(
          data = g_df,
          ggplot2::aes(
            x = x,
            y = y,
            fill = val,
            group = Months)) +
        ggplot2::facet_wrap(~Months_fct)+
        ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'RdYlBu')))+
        ggplot2::geom_sf(data=sf::st_as_sf(shapefile),
                         colour='black',
                         linetype='solid',
                         fill = NA,
                         size=0.2)+
        ggplot2::theme(
          legend.key.width = ggplot2::unit(4, 'line'),
          legend.key.height = ggplot2::unit(1, 'line'),
          legend.text = ggplot2::element_text(colour = 'white'),
          legend.background = ggplot2::element_rect(fill = 'black', colour = 'black'),
          legend.title = ggplot2::element_text(colour = 'white'),
          plot.background = ggplot2::element_rect(fill = 'black', colour = 'black'),
          strip.background = ggplot2::element_rect(fill = 'black'),
          axis.text = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x= ggplot2::element_blank(),
          axis.text.y= ggplot2::element_blank(),
          axis.ticks.y= ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = 'black'),
          strip.text = ggplot2::element_text(size = 14,family = 'sans'),

          plot.title = ggplot2::element_text(
            family = "sans",
            size = 18,
            hjust = 0.5,
            colour = 'white'),
          plot.subtitle = ggplot2::element_text(
            hjust = 0,
            size = 12),
          plot.caption = ggplot2::element_text(
            size = 10,
            hjust = 1,
            colour = 'white'),
          legend.position = "bottom"
        ) +
        ggplot2::labs(
          title = paste0("Climatology of " ,main_title.plot),
          fill = variable,
          caption = "Plot generated by Package: RWeatherTools")

      print(plot(p2))
      if (save_plot == TRUE) {

        ggplot2::ggsave(plot = p2, filename = paste0(outp,name_out,'.png'),
                        height = 9, width = 10,
                        units = 'in', dpi = 900)
      }else{
        print(plot(p2))
      }
    }

    cat('\f')
    message('Climatology for the shapefile extent has been calculated!')

  } else{

    cat('\f')
    message('Computing Climatology and plotting graph...')

    df <- tibble(
      date = seq(as.Date(star_date), as.Date(end_date), by = "1 month")
    ) %>%
      mutate(id = 1:n())

    fun.clim <- function(month, years.omit, data) {
      grd.mt <- df %>%
        dplyr::filter(
          stringr::str_sub(date, 6, 7) == month &
            stringr::str_sub(date, 1, 4) %nin% years.omit
        )

      data[[grd.mt$id]] %>%
        "*"(1) %>%
        mean(na.rm = T) %>%
        return()
    }

    grd.clim <- sapply(
      sprintf("%02d", 1:12),
      FUN = fun.clim,
      years.omit = years_omit,
      data = raster::brick(grid_path)
    ) %>%
      raster::stack() %>%
      "*"(1)

    names(grd.clim) <- month.name

    g_df <- grd.clim %>%
      raster::rasterToPoints() %>%
      as.data.frame() %>%
      `colnames<-`(c("x", "y", names(grd.clim))) %>%
      tidyr::pivot_longer(cols = 3:14,
                          names_to = "Months",
                          values_to = "val")

    myFun <- function(x, dummyDay = "", dummyYear = "2000"){
      require(lubridate)

      x <- ifelse(substr(x, 1, 3) %in% month.abb,
                  paste(match(substr(x, 1, 3), month.abb),
                        dummyDay,
                        dummyYear, sep = "/"), x)

      mdy(x)
    }

    g_df <- g_df %>%
      mutate(Date = myFun(Months),
             Months_fct = factor(Months, levels = c('January', 'February', 'March', 'April', 'May',
                                                    'June','July','August', 'September', 'October',
                                                    'November', 'December')))


    if (save_GIF == TRUE) {

      cat('\f')
      message('making gif for original grid data, this will be take some time!...')
      p <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::geom_tile(
          data = g_df,
          ggplot2::aes(
            x = x,
            y = y,
            fill = val,
            group = Months)) +
        ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'RdYlBu')))+
        ggplot2::theme(
          legend.key.width = ggplot2::unit(9, 'line'),
          legend.key.height = ggplot2::unit(2.5, 'line'),
          legend.text = ggplot2::element_text(size = 20),
          legend.title = ggplot2::element_text(size = 25),

          plot.title = ggplot2::element_text(
            family = "Prata",
            size = 40,
            hjust = 0.5),
          plot.subtitle = ggplot2::element_text(
            hjust = 0,
            size = 12),
          plot.caption = ggplot2::element_text(
            color = "grey40",
            size = 20,
            hjust = 1),
          legend.position = "bottom"
        ) +
        ggplot2::labs(
          x = 'Lontigute', y = 'Latitude',
          title = paste0("Climatology of " ,main_title.plot,"\nMonth: {closest_state}"),
          fill = variable,
          caption = "Plot generated by Package: RWeatherTools")+
        gganimate::transition_states(states = Months_fct)+
        gganimate::shadow_wake(wake_length = 0.5)

      gganimate::animate(
        p,
        width = 1200,
        height = 1200,
        renderer= gganimate::gifski_renderer(paste0(outp,name_out,'.gif'))
      )


    }else{ #si el GIF = FALSE retorna el plot normal
      cat('\f')

      p2 <- ggplot2::ggplot() +
        ggplot2::theme_dark() +
        ggplot2::geom_tile(
          data = g_df,
          ggplot2::aes(
            x = x,
            y = y,
            fill = val,
            group = Months)) +
        ggplot2::facet_wrap(~Months_fct)+
        ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'RdYlBu')))+
        ggplot2::theme(
          legend.key.width = ggplot2::unit(4, 'line'),
          legend.key.height = ggplot2::unit(1, 'line'),
          legend.text = ggplot2::element_text(colour = 'white'),
          legend.background = ggplot2::element_rect(fill = 'black', colour = 'black'),
          legend.title = ggplot2::element_text(colour = 'white'),
          plot.background = ggplot2::element_rect(fill = 'black', colour = 'black'),
          strip.background = ggplot2::element_rect(fill = 'black'),
          axis.text = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x= ggplot2::element_blank(),
          axis.text.y= ggplot2::element_blank(),
          axis.ticks.y= ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = 'black'),
          strip.text = ggplot2::element_text(size = 14,family = 'sans'),

          plot.title = ggplot2::element_text(
            family = "sans",
            size = 18,
            hjust = 0.5,
            colour = 'white'),
          plot.subtitle = ggplot2::element_text(
            hjust = 0,
            size = 12),
          plot.caption = ggplot2::element_text(
            size = 10,
            hjust = 1,
            colour = 'white'),
          legend.position = "bottom"
        ) +
        ggplot2::labs(
          title = paste0("Climatology of " ,main_title.plot),
          fill = variable,
          caption = "Plot generated by Package: RWeatherTools")

      print(plot(p2))

      if (save_plot == TRUE) {

        ggplot2::ggsave(plot = p2, filename = paste0(outp,name_out,'.png'),
                        height = 9, width = 10,
                        units = 'in', dpi = 900)
      } else{
        print(plot(p2))
      }

    }

    cat('\f')
    message('Climatology for original grid data has been calculated and exported!')

  }

  if (return_clip == TRUE ) {
    suppressWarnings(raster::writeRaster(crop, paste0(outp,name_out,".tif"), overwrite = TRUE))
    return(crop)
  }else{
    suppressWarnings(raster::writeRaster(grd.clim, paste0(outp,name_out,".tif"), overwrite = TRUE))
    return(grd.clim)
  }

}

