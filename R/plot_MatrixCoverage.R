#' @title Plot a matrix of coverage climate data (Precipitation, Discharge or Temperature) from
#' a dataframe for quality control.
#' @param data_path path directory of csv dataframe with climate data, first colum
#' It must be one of the dates and the rest of the columns the seasons with the climatic variable that you want to plot,
#' these values can be daily or monthly.
#' @param out_name output png name for the plot.
#' @param xlab changes labels of the plot on x axis.
#' @param ylab changes labels of the plot on y axis.
#' @param cex.axis size of the axis labels.
#' @param cex size of the labels.
#' @param lwd_line line thickness for the plot.
#' @rdname plot_CoverageMatrix
#' @export

plot_CoverageMatrix <- function(
    data_path,
    out_name,
    xlab = NULL,
    ylab = NULL,
    cex.axis,
    cex,
    lwd_line,
    color_line
){

  # reading data
  pr_raw  <- read.csv(data_path, sep=',', header=FALSE, encoding = "UTF-8")
  pr_abr  <- as.character(as.matrix(pr_raw[1,-1]))
  pr_time <- as.POSIXct(pr_raw[-1,1], format='%Y-%m-%d')
  pr_data <- apply(as.matrix.noquote(pr_raw[-1,-1]),2,as.numeric)

  #computing coverage
  pr_per <- c()
  for (w in 1:length(pr_abr)){
    pr_per[w] <- round((length(na.omit(pr_data[,w]))/nrow(pr_data))*100,1)
  }

  # plotting ---------------------------------------------------------------------------

  png(paste0(out_name,'_coverage_stations.png'), units="px", res=900, height=3000, width=4000)
  Sys.setlocale("LC_TIME", "C")

  # Graphical parameters <- esto es para la cantidad de datos de estaciones pluviometrica e hidrometricas
  p <- rbind(rep(1,30),rep(1,30),rep(1,30),
             rep(1,30),rep(1,30),rep(1,30),
             rep(1,30),rep(1,30),rep(1,30),
             rep(1,30),rep(1,30),rep(1,30),
             rep(1,30),rep(1,30),rep(1,30),
             rep(1,30),rep(1,30),rep(1,30),
             rep(1,30),rep(1,30),rep(1,30))

  layout(p)
  layout.show()

  par(mar=c(0,5.2,1,3.5), oma=c(3,1,0,0))
  par(mgp=c(0,0.2,0))
  par(tck=0.01)
  par(box=list(col="black", lwd=0.5))
  par(cex=0.6)

  # Plot
  x <- pr_data
  n <- ncol(x)
  for (i in 1:n){
    x[is.na(x[,i])==FALSE,i] <- rev(1:n)[i]
  }
  pr_cov  <- rev(format(pr_per, nsmall=1))
  time_at <- seq(500, nrow(x), 2000)
  x <- as.matrix(x)
  plot(1:nrow(x), as.vector(x[,1]), type='l', col = color_line, lwd=lwd_line, ylim=c(0.5, n+0.5),
       ann=F, xaxt='n', yaxt='n', yaxs='i', xaxs='i')
  for (j in 1:(n-1)){
    lines(1:nrow(x), as.vector(x[,j+1]), col = color_line, lwd=lwd_line)
  }
  axis(2, at=1:n, lab=rev(pr_abr), col.axis='black', col='black', cex.axis=cex.axis, font=3, las=2, lwd=0.5)
  axis(4, at=1:n, lab=pr_cov, cex.axis=cex.axis, las=2, col.axis='black', col='black', lwd=0.5)
  abline(v=time_at, col="gray50", lty=1, lwd=.3)
  mtext(ylab, side=2, cex=cex, line=5.2, col='black')

  #gogo
  axis(1, at=time_at, lab=format(pr_time[time_at], '%b%y'), cex.axis=cex.axis, lwd=0.55)
  mtext(xlab, side=1, cex=cex, line=1.5, col='black')
  mtext('Coverage [%]', side=4, outer=TRUE, cex=cex, line=-1.8, col='black')

  dev.off()

  cat('\f')
  message('Plot ready and saved!')


}



