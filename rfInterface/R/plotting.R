# Adam Griffin, 2019-01-08
# Plotting functions for rainfall and riverflow

#' Plot rainfall and flow time series on the same plot.
#'
#' Given an identifier for a rain measurement and/or a flow gauging station,
#' this function will plot the combined plot of rainfall and river flow, or a
#' single plot of the desired time series.
#'
#' @param rainID identifier for station recording rainfall
#' @param flowID identifier for station recording flow
#' @param rainOrg organisation responsible for the station in \code{rainID}
#' @param flowOrg organisation responsible for the station in \code{flowID}
#' @param rainDat rainfall measurement designator as described in metadata
#' @param flowDat flow measurement designator as described in metadata
#' @param startDate string of the form \code{YYYY-MM-DD} to indicate start of
#'     period desired, or single date. Whole record given if no startDate
#'     provided.
#' @param endDate string of the form \code{YYYY-MM-DD} to indicate end of
#'      period desired. If no startDate provided, this is ignored.
#' @param filepath if provided, this gives the path to save a .png image.
#'     ".'png" must be included at the end of the filepath.
#'
#' @return plots figure to open device unless \code{filepath} provided, in which
#'     case a .png file is saved instead.
#'
#' @examples
#' \dontrun{
#' rainFlowPlot(rainID = "234176", flowID = "9005",
#'              rainOrg = "SEPA", flowOrg = "NRFA",
#'              startDate = "2017-07-01", endDate = "2018-07-01")
#' }
#'
#' @export rainFlowPlot

rainFlowPlot <- function(rainID = NULL, flowID = NULL,
                           rainOrg = NULL, flowOrg = NULL,
                           rainDat = 'cdr', flowDat = 'gdf',
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
  if(!is.null(filepath)){
    grDevices::png(filepath, res=300, width=120, height=100, units='mm',
                   pointsize=10)
  }
  graphics::par(mar=c(4,3,0.4,3), mgp=c(2,1,0))



  if(length(rain_series) == 1){

    graphics::plot(as.Date(flow_series$datetime), flow_series$value,
         type='s', col='darkgreen', lwd=2,
         xlab="", ylab="GDF (cumecs)", xaxt='n')
    autoAxis(flow_series$datetime,
             cex.axis=0.6, las=2)

  }else if(length(flow_series) == 1){

    graphics::plot(as.Date(rain_series$datetime), rain_series$value,
         type='s', col='blue', lwd=2,
         xlab="", ylab="CDR (mm)", xaxt='n')
    autoAxis(rain_series$datetime,
             cex.axis=0.6, las=2)

  }else{
    graphics::par(mar=c(4,3,0.4,3), mgp=c(2,1,0))

    topY <- max(rain_series$value, na.rm=T)+max(flow_series$value, na.rm=T)
    bottomY <- signif(topY-max(rain_series$value, na.rm=T),2)
    bottomX <- as.Date(range(rain_series$datetime,
                                        flow_series$datetime))

    rainseq <- seq(topY, bottomY,
                   by=0.5*-10^floor(log10(max(rain_series$value, na.rm=T))))
    rainseq_labels <- seq(0, topY-bottomY,
                      by=0.5*10^floor(log10(max(rain_series$value, na.rm=T))))

    graphics::plot(as.Date(flow_series$datetime), flow_series$value,
         type='s', col='darkgreen', lwd=2,
         xlab="", ylab="GDF (cumecs)",
         ylim=c(0, topY),
         xlim=c(bottomX[1], bottomX[2]),
         xaxt='n')
    autoAxis(c(rain_series$datetime, flow_series$datetime),
             cex.axis=0.6, las=2)
    graphics::lines(as.Date(rain_series$datetime),
                    topY-rain_series$value, col="blue",
                    lwd=1, type='s')
    graphics::axis(4, at=rainseq, labels=rainseq_labels)
    graphics::mtext("CDR (mm)", side=4, line=2)
  }

  if(!is.null(filepath)){
    grDevices::dev.off()
  }
}

#' Add automatically scaled horizontal date axis.
#'
#' Using a provided time series, this functions adds a horizontal datetime axis
#' to an existing plot in the same fashion as the \code{axis} function. Ticks and
#' labels are automatically determined. Additional graphical \code{par} arguments
#' can be passed to customise apperance.
#'
#' @param series vector of datetime objects (readable by \code{as.Date})
#' @param ... additional \code{par} arguments to be passed to \code{axis}.
#'     Can include any argument passed to axis except \code{side}, \code{at}, \code{labels} and \code{tick}.
#'
#' @return Adds axis to existing plot in current device.
#'
#' @examples
#' \dontrun{
#'     ts <- seq(as.Date("01-01-1900"),as.Date("01-01-1902"),by="day")
#'     autoAxis(series = ts, cex.axis=0.6, las=1)
#' }
#'
#' @export autoAxis

# autoAxis systematically determines an appropriate scale for a date axis.
# series is a single datetime series object
# ... any other graphical par arguments to the axis function (see '?axis')
autoAxis <- function(series, ...){
  X <- as.Date(range(series))
  Xseq <- seq(X[1], X[2], by='month')
  Xlen <- findInterval(length(Xseq), c(0,12,36,60))
  Xseq <- switch(Xlen,
                 seq(X[1], X[2], by='month'),
                 seq(X[1], X[2], by='3 months'),
                 seq(X[1], X[2], by='6 months'),
                 seq(X[1], X[2], by='year'))
  Xlab <- switch(Xlen,
                 "%b %Y","%b %Y","%b %Y","%Y")

  graphics::axis(1, at=Xseq, labels=format(Xseq, Xlab), ...)
  graphics::axis(1, at=seq(X[1], X[2], by='month'), labels=F, tick=T)
}
