# Adam Griffin, 2019-01-16
# NRFA method for peak-over-threshold series extraction


#' Extract Peak-over-Threshold series from timeseries (NRFA criteria)
#'
#' Using a given time series, a POT series is returned using the NRFA criteria:
#' * Peaks must be seperated by a time interval at least 3 times as long as the
#' estimated time to rise at the location of interest.
#' * Between peaks, the trough must be no more than two-thirds the size of
#' either peak.
#' * All peaks must exceed a given threshold: current standard is to choose a
#' threshold which is expected to permit, on average, five events per year.
#' selecting single dates, periods of record, or entire records for single or
#' multiple sites. Metadata can also be returned for stations in the dataset.
#' All data must be of the same type from the same organisation over the
#' same period.
#'
#' @param series Either a vector of flow magnitudes, or a dataframe with two    #'     columns, magnitude and datetime.
#' @param datetime If series is not a dataframe, this is a vector of datetimes  #'       corresponding to the events in \code{series} vector.
#' @param threshold positive number describing minimum size of peak event.
#' @param timeToRise positive number describing time of rise at the location of
#'     the measurements.
#'
#' @return a list containing:
#'     * a dataframe of the peak events, including datetime and magnitude
#'     * a boolean vector the length of the original series indicating which
#'         measurements are peaks.
#'
#' @examples
#' \dontrun{
#'     POTextract(rnorm(100)+20, datetime=as.Date(1:100, origin="2000-01-01"),
#'                threshold = 20, timeOfRise = 3)
#' }
#'
#' @export import_ts

POTextract <- function(series, datetime=NULL, threshold=0, timeOfRise=0){

  if (is.data.frame(series)) {
    datetime <- series$datetime
    series <- series$value
  }else if (is.null(datetime)) {
    datetime <- 1:length(series)
  }else{
    datetime <- as.Date(datetime)
  }


  L <- length(series)

  if (length(datetime) != L) {
    stop("datetime series should match length of value series.")
  }
  datetime <- as.Date(datetime)

  is_peak <- (series[2:(L-1)] > series[1:(L-2)]) &
             (series[2:(L-1)] > series[3:L]) &
             (series[2:(L-1)] > threshold)
  is_peak <- c((series[1] > series[2]) & (series[1] > threshold),
               is_peak,
               (series[L] > series[L-1]) & series[L] > threshold)
  # include endpoints
  NP <- sum(is_peak)


  peaks<- which(is_peak)
  # peaktimes <- datetime[is_peak]  # times of peaks
  # peakmags <- series[is_peak]  # sizes of peaks


  pot <- data.frame(list(datetime = datetime[peaks],
                         value    = series[peaks],
                         pos = peaks))


  if (NP < 1) {
    message("Series is constant. No peaks.")
    return(data.frame(list(datetime=c(), value=c())))

  }else{
    start <- 1
    if (!is_peak[1]) {
      peaks <- c(series[1], peaks)  # dummy peak
      start <- start + 1
    }
    if (!is_peak[L]) {
      peaks <- c(peaks, series[L])  # dummy peak
      start <- start + 1
    }

    peaks_order <- peaks[order(peaks)] #potential peaks from smallest to biggest


    for (p in start:NP) { # don't include dummy peaks
      #print(pot$pos)
      npo <- order(pot$pos)[p]  # chronological order of p'th largest peak

      # time spacing
      dt_pre <- pot$datetime[npo] - pot$datetime[npo-1]
      dt_post <- pot$datetime[npo+1] - pot$datetime[npo]

      if(min(dt_pre, dt_post) < 3*timeOfRise){
        pot <- pot[-npo, ]
        is_peak[pot$pos[npo]] <- F
        next
      }

      #inter-peak drop
      min_pre <- min(series[pot$pos[npo - 1]:pot$pos[npo]])
      min_post <- min(series[pot$pos[npo]:pot$pos[npo + 1]])
      if(min_pre > (2/3) * pot$value[npo] | min_post > (2/3) * pot$value[npo]){
        pot <- pot[-npo, ]
        is_peak[pot$pos[npo]] <- F
        next
      }
    }
  }
  return(list(df=pot, is_peak=is_peak))
}
