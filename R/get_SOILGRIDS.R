#' @title donwload_SOILDGRIDS
#' @description Download SOILGRIDS datasets V2 for a roi area.
#' @param path_saved character string to path directory where the data will save.Default: getwd()
#' @param variables character string of variables what you want to download. Default: c("silt", "clay", "sand", "wrb")
#' @param layers depth of the layers. Default: c("0-5cm_mean")
#' @param roi vector of Longitude min. longitude max, latitude min and latitude max. Default: c(-86, -66, -20, 2) for Peru area.
#' @return download tifs of variables request.

#' @examples
#' \dontrun{
#' if(interactive()){
#'  donwload_SOILDGRIDS(path_saved = 'C:/RPackages/hydClimR/usefull_data/SOILSGRID',
#'                      variables = c("silt", "clay", "sand", "wrb"),
#'                      layers = c('0-5cm_mean','5-15cm_mean'))
#'  }
#' }
#' @rdname donwload_SOILDGRIDS
#' @export

donwload_SOILDGRIDS <- function(
    path_saved = getwd(),
    variables = c("silt", "clay", "sand", "wrb"),
    layers = c('0-5cm_mean'),
    roi = c(-86,-66,-20,2)
  ){

  #this functions was adapted from https://github.com/zecojls/downloadSoilGridsV2/blob/master/script_soilGrids_download.R
  variales_all <- c("wrb", "phh2o", "soc", "nitrogen",
                    "cec", "silt", "clay", "sand","bdod")
  donw_vars <- variales_all[variales_all %in% variables]
  donw_vars <- paste0(donw_vars,'.map')
  stopifnot("Variabe ID doesn't exits..." = variables %in% variales_all)

  layers_all  <- c("0-5cm_mean", "5-15cm_mean",
                  "15-30cm_mean","30-60cm_mean")
  donw_layers <- layers_all[layers_all %in% layers]
  stopifnot("Layer don't found, please if you are triying to put just e.g ('0-5cm),add '_mean'" = layers %in% layers_all)

  ##bounding boxes
  seq.long <- seq(roi[1]*1.2, roi[2]*1.2, by = 0.2)
  seq.lat  <- seq(roi[3]*1.2, roi[4]*1.2, by = 0.2)

  grid_min <- expand.grid(seq.long[-length(seq.long)], seq.lat[-length(seq.lat)])
  grid_max <- expand.grid(seq.long[-1], seq.lat[-1])

  full.combination <- tibble(min.long = grid_min[,1],
                             max.long = grid_max[,1],
                             min.lat = grid_min[,2],
                             max.lat = grid_max[,2])

  full.combination <- full.combination %>%
    mutate(min.long = min.long - 0.01,
           max.long = max.long + 0.01,
           min.lat = min.lat - 0.01,
           max.lat = max.lat + 0.01)

  full.combination <- as.data.frame(full.combination)
  bbox.coordinates <- full.combination %>%
    mutate(left.coord = paste0(ifelse(min.long < 0, "W", "E"), round(abs(min.long), 0)),
           top.coord = paste0(ifelse(max.lat < 0, "S", "N"), round(abs(max.lat), 0)))

  cat('\f')

  for(a in 1:length(donw_vars)) {

    donw_vars_dw <- donw_vars[a]

    donw_vars.prefix <- gsub(".map", "", donw_vars_dw)

    if(donw_vars_dw == "wrb.map") {

      layer <- "MostProbable"

      for(t in 1:nrow(bbox.coordinates)) {

        min.long = bbox.coordinates[t,"min.long"]
        max.long = bbox.coordinates[t,"max.long"]
        min.lat = bbox.coordinates[t,"min.lat"]
        max.lat = bbox.coordinates[t,"max.lat"]
        left.coord <- bbox.coordinates[t,"left.coord"]
        top.coord <- bbox.coordinates[t,"top.coord"]

        wcs <- paste0("https://maps.isric.org/mapserv?map=/map/", donw_vars_dw, "&",
                      "SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=", layer, "&",
                      "FORMAT=image/tiff&",
                      "SUBSET=long(", min.long, ",", max.long, ")&",
                      "SUBSET=lat(", min.lat, ",", max.lat, ")&",
                      "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326")

        destination.file <- paste0(path_saved, "/SoilGrids_",
                                   paste(donw_vars.prefix, layer,
                                         left.coord, top.coord, sep = "_"),
                                   ".tif")

        if(file.exists(destination.file)) {

          next

        } else {
          cat('\f')
          cat("Downloading: ",basename(destination.file), "\n")
          download.file(wcs, destfile = destination.file, mode = 'wb',quiet = TRUE)

        }

      }

    } else {

      for(l in 1:length(layers)) {

        layer <- layers[l]

        for(t in 1:nrow(bbox.coordinates)) {

          min.long = bbox.coordinates[t, "min.long"]
          max.long = bbox.coordinates[t, "max.long"]
          min.lat = bbox.coordinates[t, "min.lat"]
          max.lat = bbox.coordinates[t, "max.lat"]
          left.coord <- bbox.coordinates[t, "left.coord"]
          top.coord <- bbox.coordinates[t, "top.coord"]

          wcs <- paste0("https://maps.isric.org/mapserv?map=/map/", donw_vars_dw, "&",
                        "SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=", donw_vars.prefix, "_", layer, "&",
                        "FORMAT=image/tiff&",
                        "SUBSET=long(", min.long, ",", max.long, ")&",
                        "SUBSET=lat(", min.lat, ",", max.lat, ")&",
                        "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326")

          destination.file <- paste0(path_saved, "/SoilGrids_",
                                     paste(donw_vars.prefix, layer,
                                           left.coord, top.coord, sep = "_"),
                                     ".tif")

          if(file.exists(destination.file)) {

            next

          } else {
            cat('\f')
            cat("Downloading: ", basename(destination.file), "\n")
            download.file(wcs, destfile = destination.file, mode = 'wb', method = 'curl')

          }
        }
      }
    }
  }




}



