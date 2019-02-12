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
  #datetime <- as.Date(datetime)

  is_peak <- (series[2:(L-1)] > series[1:(L-2)]) &
    (series[2:(L-1)] > series[3:L]) &
    (series[2:(L-1)] > threshold)
  is_peak <- c(F, is_peak, F)
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
    return(list(df=NA, is_peak=is_peak))

  }else{

    start <- 3
    end <- NP+2

    pot <- rbind(data.frame(datetime = datetime[1],
                            value = series[1],
                            pos = 1),
                 pot) # dummy peak
    pot <- rbind(pot,
                 data.frame(datetime = datetime[L],
                            value = series[L],
                            pos = L)) # dummy peak

    peaks_order <- peaks[order(peaks)] #potential peaks from smallest to biggest

    p <- start
    while (p <= nrow(pot)) { # don't include dummy peaks
      #print(pot$pos)
      npo <- order(pot$value)[p]  # chronological order of p'th largest peak

      # time spacing
      dt_pre <- as.numeric(pot$datetime[npo] - pot$datetime[npo-1])
      dt_post <- as.numeric(pot$datetime[npo+1] - pot$datetime[npo])

      if(min(dt_pre, dt_post) < 3*timeOfRise){
        pot <- pot[-npo, ]
        is_peak[pot$pos[npo]] <- F
        next #if too close, drop
      }

      #inter-peak drop
      min_pre <- min(series[pot$pos[npo - 1]:pot$pos[npo]])
      min_post <- min(series[pot$pos[npo]:pot$pos[npo + 1]])
      if(min_pre > (2/3) * pot$value[npo] | min_post > (2/3) * pot$value[npo]){
        pot <- pot[-npo, ]
        is_peak[pot$pos[npo]] <- F
        next  # if no trough, drop
      }
      p <- p+1 #if kept, move onto next peak anyway
      points(pot$datetime, pot$value, col=p)
    }
    pot <- pot[2:(nrow(pot)-1),]
  }
  points(pot$datetime, pot$value, col=2, pch=4)
  return(list(df=pot, is_peak=is_peak))
}


xt <- exp(arima.sim(list(order=c(3,0,0), ar=c(0.5,0.25,0.125)), n=100) + 2)
dt <- as.Date(1:100, origin="2000-01-01")
plot(dt,xt, type='l', xaxt='n')
autoAxis(dt)
#debug(POTextract)
pot <- POTextract(xt, dt, threshold = quantile(xt,0.9), timeOfRise=3)

xt <- exp(arima.sim(list(order=c(3,0,0), ar=c(0.5,0.25,0.125)), n=100) + 2)
potx <- ilaprosUtils::extractPeaks(1:100,xt,5,(2/3))
poty <- extractPeaksMod(1:100,xt,5,(2/3))
plot(dt,xt, type='l', xaxt='n')
points(dt[potx==1], xt[potx==1], col='blue', pch=3)
points(dt[poty==1], xt[poty==1], col='red', pch=2)

xt <- rep(70,100)
xt[70] <- 100
xt[85] <- 90
#xt[50] <- 300
poty <- extractPeaksMod(1:100,xt,5,(2/3))


#
extractPeaksMod <- function (vecTime = NULL,
                             vecObs,
                             mintimeDiff = 73,
                             thrConst = (2/3)) {
  if (is.null(vecTime)) {
    vecTime <- seq_along(vecObs)
  }
  if (!is.numeric(vecTime)) {
    warning("Time should be numeric")
  }

  #setup
  tt <- as.data.frame(cbind(vecTime, vecObs))
  names(tt) <- c("time", "obs")
  tt <- tt[order(tt$time), ]

  #find all the peaks and troughs
  cc <- ilaprosUtils:::turnpoints(tt$obs)
  sub <- data.frame(time = tt$time[cc$pos],
                    flow = tt$obs[cc$pos],
                    peak = cc$peaks,
                    pit = cc$pits)

  names(sub) <- c("time", "flow", "peak", "pit")
  keep <- rep(0, nrow(sub))
  ObsEA <- seq(1, nrow(tt))[-cc$pos]
  obsPeaks <- (seq(1, nrow(sub))[sub$peak == 1])
  obsPits <- (seq(1, nrow(sub))[sub$pit == 1])
  keep[obsPeaks[1]] <- 1

  #peak validation
  for (i in 2:length(obsPeaks)) {
    now <- obsPeaks[i]
    prev <- max(obsPeaks[1:(i - 1)][obsPeaks[1:(i - 1)] %in%
                                      seq(1, length(keep))[keep == 1]])
      # which obsPeak most recently has been kept
    keep[now] <- 1
    tp <- sub$time[prev]
    fp <- sub$flow[prev]
    tn <- sub$time[now]
    fn <- sub$flow[now]
    if (fn == fp)
      smallest <- prev
    else smallest <- c(prev, now)[!c(prev, now) %in%
                                    seq(prev, now)[max.col(t(sub[prev:now, 2]))]]
    if ((tn - tp) < mintimeDiff)
      keep[smallest] <- 0
    else {
      minThrough <- min(sub$flow[obsPits[obsPits < now &
                                           obsPits > prev]])
      if (minThrough > min(fp, fn) * (thrConst))
        keep[smallest] <- 0
    }
  }

  # formatting for output
  isPeak <- rep(0, nrow(tt))
  isPeak[cc$pos] <- keep
  isPeak[ObsEA] <- 0
  isPeak
}
