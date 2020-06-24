# Adam Griffin, 2019-01-16
# NRFA method for peak-over-threshold series extraction


#' Extract Peak-over-Threshold series from timeseries (NRFA criteria)
#'
#' Using a given time series, a POT series is returned using the NRFA criteria:
#' \itemize{
#' \item Peaks must be seperated by a time interval at least 3 times as long as the
#' estimated time to rise at the location of interest.
#' \item Between peaks, the trough must be no more than two-thirds the size of
#' either peak.
#' \item All peaks must exceed a given threshold: current standard is to choose a
#' threshold which is expected to permit, on average, five events per year.
#' }
#' This is based on the work of Ilaria Prosdocimi and the packages \code{pastecs}
#' and \code{ilaProsUtils}.
#'
#' @param series Either a vector of flow magnitudes, or a dataframe with two
#'       columns, magnitude and datetime.
#' @param datetime If series is not a dataframe, this is a vector of datetimes
#'       corresponding to the events in \code{series} vector. If not supplied,
#'       and no datetime supplied, ordinal values are assumed (1,2,...,N).
#' @param threshold positive number describing minimum size of peak event. If
#'       not supplied, takes the 90th percentile of the data.
#' @param timeOfRise positive number describing time of rise at the location of
#'       the measurements.
#'
#' @return a list containing:
#'     \itemize{
#'     \item a dataframe of the peak events, including datetime and magnitude
#'     \item a boolean vector the length of the original series indicating which
#'         measurements are peaks.
#'     }
#'
#' @examples
#' \dontrun{
#'     xt <- (arima.sim(list(order=c(3,0,0), ar=c(0.5,0.25,0.125)), n=100) + 2)^2
#'     dt <- as.Date(1:100, origin="2000-01-01")
#'     extractPOT(xt, dt, threshold = 20, timeOfRise = 3)
#' }
#'
#' @export
extractPOT <- function(series, datetime=NULL, threshold=0, timeOfRise=0){

  thrConst <- 2/3

  if (!is.data.frame(series)) {
    if (is.null(datetime)){
      datetime_real <- 1:length(series)
    }
    datetime_real <- datetime
    series0 <- data.frame(datetime=1:length(series), series=series)
  } else {
    datetime_real <- sort(series0[, 1]) # sorted dates
    series0$datetime <- order(series0[, 1]) #numerical 1:N
  }
  series <- series0

  mintimeDiff <- 3*timeOfRise

  #setup
  tt <- series
  names(tt) <- c("time", "obs")
  tt <- tt[order(tt$time), ]

  NR <- nrow(tt)

  #find all the peaks and troughs
  cc <- pastecs::turnpoints(tt$obs)  # cc$pos is timepoints without tied values
  NP <- length(cc$pos)
  sub <- data.frame(time = tt$time[cc$pos],
                    flow = tt$obs[cc$pos],
                    peak = cc$peaks,  # peak = 1, nonpeak = 0
                    pit = cc$pits)  # pits = troughs
  sub$peak[sub$flow < threshold] <- 0  # remove peaks below threshold
  keep <-     rep(0, NP)
  ObsEA <-    (1:NR)[-cc$pos]
  obsPeaks <- which(sub$peak == 1)
  obsPits <-  which(sub$pit  == 1)
  keep[obsPeaks[1]] <- 1  # keep first peak to start with

  #peak validation
  for (i in 2:length(obsPeaks)) {
    now <- obsPeaks[i]
    prev <- max(intersect(obsPeaks[1:(i-1)], which(keep==1)))
    # which still remaining obsPeak occured most recently
    keep[now] <- 1
    tp <- sub$time[prev]
    fp <- sub$flow[prev]
    tn <- sub$time[now]
    fn <- sub$flow[now]
    if (fn == fp) {
      smallest <- prev
    }else{
      smallest <- ifelse(sub$flow[prev] < sub$flow[now], prev, now)
    }
    if ((tn - tp) < mintimeDiff) {
      keep[smallest] <- 0
    }else{
      minThrough <- min(sub$flow[(prev+1):(now-1)])
      if (minThrough > (min(fp, fn) * thrConst)) {
        keep[smallest] <- 0
      }
    }
  }

  # formatting for output
  isPeak <- rep(0, NR)
  isPeak[cc$pos] <- keep
  isPeak[ObsEA] <- 0

  pot <- tt[isPeak == 1, ]
  pot[, 1] <- datetime_real[isPeak == 1]

  return(list(is_peak=isPeak, pot=pot))
}
