# Adam Griffin, 2019-01-08
# Plotting functions for rainfall and riverflow

rain_series <- import_ts("234176", dat="cdr", org="SEPA",
                              startDate = "2017-07-01", endDate = "2018-07-01",
                              datetime=T)$data

# idConverter("234176", "SEPA")
# [1] 9005

flow_series <- import_ts("9005", dat="gdf", org="NRFA",
                              startDate = "2017-07-01", endDate = "2018-07-01",
                              datetime=T)$data

library(ggplot2)
g <- ggplot(rain_plot_test_ts$data, aes(datetime, value)) +
     geom_line(color='blue') +
     scale_x_datetime("Date", date_breaks="1 month", date_labels="%b")
g



plot(river_plot_test_ts$data)

rain_flow_plot <- function(rainID = NULL, flowID = NULL,
                           rainOrg = NULL, flowOrg = NULL,
                           rainDat = NULL, flowDat = NULL,
                           startDate = NULL, endDate = NULL,
                           filepath = NULL){

  if(is.null(rainID) & is.null(flowID)){
    stop("At least one station must be provided.")
  }

  rain_series <- F
  flow_series <- F

  if(!is.null(rainID)){
    if(is.null(rainOrg) | is.null(rainDat)){
      stop("Organisation and Data type required for rain station.")
    }else{
      rain_series <- import_ts(rainID, dat=rainDat, org=rainOrg,
                               startDate = startDate, endDate = endDate,
                               datetime=T)$data
    }
  }

  if(!is.null(flowID)){
    if(is.null(flowOrg) | is.null(flowDat)){
      stop("Organisation and Data type required for rain station.")
    }else{
      flow_series <- import_ts(flowID, dat=flowDat, org=flowOrg,
                               startDate = startDate, endDate = endDate,
                               datetime=T)$data
    }
  }

  png(filepath, res=300, width=120, height=100, units='mm', pointsize=10)
  par(mar=c(4,3,0.4,3), mgp=c(2,1,0))

  if(rain_series == F){
    par(mar=c(4,3,0.4,3), mgp=c(2,1,0))
    plot(flow_series, type='l', col='black', lwd=2,
         xlab="Date", ylab="GDF (cumecs)")
  }else if(flow_series == F){
    par(mar=c(4,3,0.4,3), mgp=c(2,1,0))
    plot(rain_series, type='l', col='black', lwd=2,
         xlab="Date", ylab="CDR (mm)")
  }else{
    par(mar=c(4,3,0.4,3), mgp=c(2,1,0))
    topY <- max(rain_series$value, na.rm=T)+max(flow_series$value, na.rm=T)
    bottomY <- signif(topY-max(rain_series$value, na.rm=T),2)

    bottomX <- as.Date(range(rain_series$datetime, flow_series$datetime))
    topX <- max(rain_series$datetime, flow_series$datetime)

    rainseq <- seq(topY, bottomY,
                   by=-10^floor(log10(max(rain_series$value, na.rm=T))))
    rainseq_labels <- seq(0, topY-bottomY,
                          by=10^floor(log10(max(rain_series$value, na.rm=T))))

    plot(as.Date(flow_series$datetime), flow_series$value,
         type='l', col='darkgreen', lwd=2,
         xlab="", ylab="GDF (cumecs)",
         ylim=c(0, topY),
         xlim=c(bottomX[1], bottomX[2]),
         xaxt='n')
    axis(1, at=seq(bottomX[1], bottomX[2], by='month'),
         labels=format(seq(bottomX[1], bottomX[2], by='month'), format="%b %Y"), cex.axis=0.6, las=2)
    lines(as.Date(rain_series$datetime), topY-rain_series$value, col="blue",
          lwd=1)
    axis(4, at=rainseq, labels=rainseq_labels)
    mtext("CDR (mm)", side=4, line=2)
  }

}
