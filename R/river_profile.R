#' @title create an elevation profile of a river from a Digital Elevation Model.
#' @param path_dem path directory of the digital elevation model (DEM).
#' @param path_river path directory of the river shapefile what you want to create the river profile elevation.
#' @param river_name character of name of the river what your are computting profile.
#' @param pt_per_km Distance for extract values from DEM in km, Default: 1
#' @param spanish_labels logical, If \code{TRUE} this will create the plot in spanish labe, Default: FALSE
#' @examples
#' \dontrun{
#' if(interactive()){
#'## computting river profile using the function
#'pt_per_km  <- 5
#'river_name <- 'Río Cajamarquino'
#'path_river <- 'test_data/pruebas/Rio_Cajamarquino.shp'
#'path_dem   <- 'test_data/pruebas/DEM_alos_JT.tif'
#'
#'p_river <- river_profile(
#'
#'  path_dem = path_dem,
#'  path_river = path_river,
#'  river_name = river_name,
#'  pt_per_km = pt_per_km,
#'  spanish_labels = TRUE
#'
#')
#'
#'print(p_river)
#'  }
#' }
#' @rdname river_profile
#' @export
#' @importFrom stars read_stars st_extract
#' @importFrom units set_units
#' @importFrom dplyr mutate lag row_number lead
#' @importFrom tidyr replace_na
#' @importFrom RColorBrewer brewer.pal

river_profile <- function(
    path_dem,
    path_river,
    river_name,
    pt_per_km = 1,
    spanish_labels = FALSE
){

  river <- sf::st_read(path_river) %>%
    sf::st_as_sf()

  DEM <- stars::read_stars(path_dem)

  # Make Sample Points
  my_points <- river %>%
    st_line_sample(density = units::set_units(pt_per_km, 1/km)) %>%
    st_cast("POINT") %>%
    st_as_sf() %>%
    st_transform(st_crs(DEM))

  # Extract DEM Values at Points
  my_points_dem <- DEM %>%
    stars::st_extract(my_points)  %>%
    dplyr::mutate(dist_seg_m = tidyr::replace_na(as.numeric(st_distance(x, dplyr::lag(x), by_element = TRUE)),0),
                  dist_tot_m = cumsum(dist_seg_m),
                  id = dplyr::row_number(),
                  river_name = river_name)

  my_points_dem <- my_points_dem %>%
    dplyr::mutate(
      elevation = my_points_dem[[1]]
    )

  dat <- my_points_dem %>%
    dplyr::mutate(slope = (
      (dplyr::lag(elevation,2)-dplyr::lead(elevation,2))/
        (dplyr::lag(dist_tot_m,2)-dplyr::lead(dist_tot_m,2)))*100)


  if (spanish_labels == TRUE) {

      gg <- dat %>%
        ggplot() +
        geom_point(aes(dist_tot_m/1000, elevation, color = slope), show.legend = T) +
        scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                              guide = guide_colourbar(barheight = 10, frame.colour = "black", ticks.colour = "black")) +
        labs(x = "Distancia (km)", y = "Elevación (m)", title = paste0(river_name,' perfil de elevación y pendientes'), color = "Pendiente (%)") +
        theme_bw() +
        theme(aspect.ratio = 0.4)

    }else{

      gg <- dat %>%
        ggplot() +
        geom_point(aes(dist_tot_m/1000, elevation, color = slope), show.legend = T) +
        scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                              guide = guide_colourbar(barheight = 10, frame.colour = "black", ticks.colour = "black")) +
        labs(x = "Distance (km)", y = "Elevation (m)", title = paste0(river_name,' elevation profile and slope'), color = "Slope (%)") +
        theme_bw() +
        theme(aspect.ratio = 0.4)

    }

    ggsave(plot = gg,
           filename = paste0(getwd(),'/river profile of ',river_name,'.png'),
           width = 7,
           height = 3,
           units = 'in',
           dpi = 700)

    cat('\f')
    message('Done and plot saved!')
    return(gg)

}


