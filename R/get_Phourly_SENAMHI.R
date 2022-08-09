get_hourlyCODES_SENAMHI <- function(region = NULL, return_codes_only = FALSE){

  up1 <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQSeMoQOW-cuqA3Fes6CyAcEewqpBku1-su7zl'
  key <-  'KBa84Chf8xGZtM734hZJaO4Hz748Ohx94317NK3Gy/pub?gid=0&single=true&output'

  ur <- paste0(up1,key)
  CODES  <- readr::read_csv(paste0(ur,'=csv'), col_types = '')
  CODES_region <- CODES[CODES$Region %in% region, ]

  if (return_codes_only == TRUE) {
    CODES_only <- CODES_region[,2]
    return(purrr::as_vector(CODES_only))
  }else{
    return(CODES_region)
  }

}


# tumbes <- get_hourlyCODES_SENAMHI(region = 'TUMBES', TRUE)

get_hourly_SENAMHI <- function(

    dsn = getwd,
    codes = NULL,
    start_date = NULL,
    end_date = NULL
){

  codes <- tumbes
  Time_ini_obs  <- paste0(as.Date(start_date),' 01:00')
  Time_end_obs  <- paste0(as.Date(end_date),' 00:00')



  up1 <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQSeMoQOW-cuqA3Fes6CyAcEewqpBku1-su7zl'
  key <-  'KBa84Chf8xGZtM734hZJaO4Hz748Ohx94317NK3Gy/pub?gid=0&single=true&output'
  ur <- paste0(up1,key)
  CODES  <- readr::read_csv(paste0(ur,'=csv'), col_types = '')

  filter_data <- CODES[CODES$StationID %in% codes, ]

  library(XML)
  library(httr)
  # library(dplyr)

  cod   <- as.vector(codes)
  nom   <- as.vector(filter_data$Station)

  # Agrupar datos de estaciones
  ini    <-  as.POSIXlt(Time_ini_obs)
  end    <-  as.POSIXlt(Time_end_obs)
  subtime <-  seq(ini, end, by="hour")
  subdata <- matrix(NA,length(subtime),length(cod))

  for (i in 1:length(cod)){
    ur    <-  paste0('https://www.senamhi.gob.pe/site/lvera/lluvia2.php?DATO1=',cod[i])
    ur2   <- paste0('https://www.senamhi.gob.pe/site/lvera/exportar_excel_pp.php?DATO1=',
                    cod[i],'&MESI1=01&nomi=PP_CAMPAMENTO%20SEDE_2016_01.xls')
    tabs   <-  GET(ur)
    tabla  <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)[[2]]


    if (dim(tabla)[1]>3){ #Verifica si la estacion está operativa

      tabla   <- na.omit(tabla)
      tweb    <- na.omit(tabla[,3:5])
      n       <- dim(tweb)[1]
      time.url<- seq(as.POSIXlt(paste0(tweb[1,1],'-',tweb[1,2],'-',tweb[1,3],' 01:00')),
                     as.POSIXlt(paste0(tweb[n,1],'-',tweb[n,2],'-',tweb[n,3],' 00:00'))+3600*24,
                     by="hour")

      indx    <- which(time.url==ini)
      indx2   <- which(time.url==end)

      if (length(indx)==0 || length(indx2)==0){#Verifica si las fechas selecc están OK
        print("Se seleccionó mal el rango de fecha!!")
      } else {
        subset   <- seq(indx,indx2)
        pp <- as.numeric(t(as.matrix(tabla[,6:29])))
        subdata[,i]  <- pp[subset]
      }
    }
  }

  # Exportar base de datos
  if (length(indx)==0 || length(indx2)==0){
    print("Se seleccionó mal el rango de fecha!!")
  } else {

    #values <- rbind(nom, lat, lon, alt, subdata)
    #labels <- c('Station', 'Lat', 'Lon', 'Elev', as.character(subtime))
    values <- rbind(cod, subdata)
    labels <- c('Station', as.character(subtime))
    export <- cbind(labels, values)
    write.table(export, file='PpObs_Act.csv', sep=',', row.names=F, col.names=F)

  }
}




