#' Return Mann-Kendall test statistics
#'
#' Given a time-series vector, return the test statistic for the adjusted
#' Mann-Kendall test as in the \code{Kendall} package. This allows for repeated
#' values.
#'
#' @param x time-series vector of observations.
#'
#' @return Z, the test statistic for Mann-Kendall
#'
#' @export
KendallZScore <- function(x) {
  Y <- outer(x,x,function(x,y){sign(x-y)})
  S <- sum(Y[lower.tri(Y)])
  varS <- length(x)*(length(x)-1)*(2*length(x)+5)/18
  tx <- tabulate(x)
  if(any(tx>1)){
    ties <- tx[tx>1]
    varS <- varS - sum(ties*(ties-1)*(2*ties+5))/18
  }
  #Calculate the Zs statistic
  Zs <- ifelse(abs(S) < 1e-10, 0, (S - sign(S))/sqrt(varS))
  Zs  # MkZs
}


#' Modified autocorrelation function
#'
#' Computes the lag-1 autocorrelation of a time-series, and returns its value and
#' whether it is significant to a given level.
#'
#' @param x a univariate time-series object
#' @param sig.level value in [0,1] describing level of significance
#'
#' @return a vector containing the lag-1 autocorrelation, and a boolean indicating
#'  significance.
#'
#' @export
ACFmod <- function (x, sig.level = 0.95) {
  # Extract lag1 acf value
  acf_table <- stats::acf(x, plot=FALSE, na.action=stats::na.pass) # no plot and allow missing values
  lag1 <- acf_table[[1]][2]  # double brackets to access values in a list
  # Add significance routine
  # 95% significance level calculated as +- 1.96/sqrt(n), where n = length of x
  sig.level <- stats::qnorm(1 - 0.5*(1-sig.level))
  sig <- 0 # initiate sig
  if (lag1 > (sig.level / sqrt(sum(!is.na(x))))) { # sum(!is.na(x)) instead of length (x) - see notes.
    sig <- 1  # Significant positive lag1
  } else if (lag1 < -1 * (sig.level / sqrt(sum(!is.na(x))))) {
    sig <- 0 # Significant negative lag1 (note change above)
  }
  return(c(lag1, sig))
}


#' Detrended time-series using Theil Sen estimates of trend.
#'
#' Given a time-series with values and time points, returns detrended time-series and
#' residuals from linear regression. Also returns the a dataframe of the detrended time-series
#' under Theil-Sen.
#'
#' @param x numeric vector of timepoints
#' @param y numeric vector of observations
#'
#' @return a list containing: three dataframes,
#'    \itemize{
#'    \item a dataframe with two columns, timepoints and detrended values under linear regression
#'    \item a dataframe with two columns, timepoints and residuals under linear regression
#'    \item a dataframe with two columns, timepoints and detrended values under Theil-Sen estimation.
#'    }
#'
#' @export
detrend <- function (x,y) {
  # Linear Regression detrend -------------------------------------------
  lm.output <- stats::lm(y~x)
  lm.slope <- lm.output$coefficients[[2]] # Extract slope from list
  Yt.lr <- cbind(x,(y - lm.slope*(1:length(y))))
  # Linear regression residuals -----------------------------------------
  Yt.lr.res <- cbind(x[!is.na(y)],lm.output$residuals) # for keeping only the non-NA
  # Theil-Sen detrend ---------------------------------------------------
  # Calculate the Sen Slope (y = data, x = year!! - Don't put other way around)
  # Extract the "slope" coefficients and put in col 3 in matrix
  TSA.slope <- zyp::zyp.sen(y~x)$coefficients[2] # coeif 2 = slope
  # Create vector for detrend data (Yt = Xt - B*t)
  # Calculation to detrend data (Yt = Xt - B*t (where t = 1:58))
  Yt.sen <- cbind(x, (y - (TSA.slope * (1:length(y)))))
  return(list(lr=Yt.lr, lr_res=Yt.lr.res, sen=Yt.sen)) # Return
}


#' Relative Theil Sen Estimator
#'
#' Computes the relative Theil-Sen Estimator of a time-series which is given by \eqn{\xi N / \mu}
#' where \eqn{\xi} is the standard TSE, and \eqn{\mu} the mean of the time-series.
#'
#' @param x numeric vector of timepoints
#' @param y numeric vector of observations
#'
#' @return the value of relative TSE.
#'
#' @export
relativeTheilSen <- function(x, y) {
  TSA.slope <- zyp::zyp.sen(y~x)$coefficients[2]
  # Calc relative TSA (e.g. Stahl et al., 2012)
  X_bar <- mean(y, na.rm = T)
  # use absolute mean, in case there are negatives values (happens at dummy ones)
  rel.TSA <- ((TSA.slope * (length(x))) / abs(X_bar)) * 100
  return(rel.TSA)
}


#' Modified autocorrelation function on detrended data
#'
#' Detrends a time-series using Theil-Sen and computes the lag-1 autocorrelation,
#' and returns its value and whether it is significant to a given level.
#'
#' @param x numeric vector of timepoints
#' @param y numeric vector of observations
#' @param sig.level level of significance to test at.
#'
#' @return a vector of 2 values describing lag-1 autocorrelation of the detrended series,
#' and whether the autocorrelation is significant.
#'
#' @export
ACFdetrended <- function (x, y, sig.level = 0.95) {

  dtr.sen  <-  detrend(x, y)$sen # detrended with TSA slope
  ACF.lag1 <- ACFmod(dtr.sen[ ,2], sig.level=sig.level) # actual lag1 correlation
  return(ACF.lag1)
}

#' FEH Generalised Logistic Distribution
#'
#' Density and Quantile functions for the FEH 3-parameter generalised Logistic
#' distribution, as appears in \code{ilaprosUtils}.
#'
#' @param x,p a vector of values or probabilities of exceedence
#' @param loc location parameter
#' @param scale scale parameter > 0
#' @param sh shape parameter \eqn{ \xi \in [-1,1]}
#' @param lower.tail if true, computes the quantile for \eqn{P[X < x] = p}
#'
#' @returns evaluation of density and quantile function
#'
#' @export
dglo <- function (x, loc, scale, sh) {
  if (sh > -1e-07 & sh < 1e-07) {
    tx <- (x - loc)/scale
  }
  else {
    tx <- (-1/sh) * log(1 - sh * (x - loc)/scale)
  }
  (1/scale) * exp(-(1 - sh) * tx)/((1 + exp(-tx))^2)
}

#' @rdname dglo
#' @export
qglo <- function (p, loc, scale, sh, lower.tail = TRUE) {
  if (!lower.tail)
    p <- 1 - p
  if (sh > -1e-07 & sh < 1e-07)
    loc - scale * (log((1 - p)/p))
  else loc + (scale * (1 - ((1 - p)/p)^(sh)))/sh
}


#' Compute hydrological year and day-of-year
#'
#' Return the hydrological year and day-of-year. The UK hydrological year starts
#' at 9am on 1st October. For example, the 1901-1902 hydrological year is denoted 1902.
#' The hydrological day that starts at 9am, 4th October is denoted DOY = 4.
#' @param datetime vector of datetime objects
#' @return A dataframe of two columns, DOY and hydrological year.
#'
#' @examples \dontrun{
#' datetimes(as.POSIXlt((1:365+0.25)*76400, origin="1990-01-01"))
#' }
#' @export
findHydrolYr <- function (datetime) {
  # returns the hydrological year and DOY (starts at 9am, 1st October)
  # e.g. the 2018-19 hydrol year is denoted 2019.
  # the hydrol day that starts at 9am 3rd May is denoted 3rd May (or DOY equiv)
  dt <- lubridate::ymd_hms(datetime)
  hms_flag <- TRUE
  if(all(is.na(dt))){
    dt <- lubridate::ymd(datetime)
    hms_flag <- FALSE
  }
  wd <- dt
  if(hms_flag){
    wd[which(lubridate::hour(dt) < 9)] <- wd[which(lubridate::hour(dt) < 9)] - lubridate::days(1)
  }
  y <- lubridate::year(wd)
  y[which(lubridate::month(wd) > 9)] <- y[which(lubridate::month(wd) > 9)] + 1
  if(hms_flag){
    startwy <- lubridate::ymd_hms(paste0(y-1,"-10-01 09:00:00"))
  }else{
    startwy <- lubridate::ymd(paste0(y-1,"-10-01"))
  }
  nday <- as.numeric(dt - startwy)
  return(data.frame(DOY=nday, yr=y))
}

#' Maximum likelihood estimation of non-stationary distributions using TSE
#'
#' fits the GLO parameters as described in FEH Vol III,
#' assuming a linear trend in the location parameter only, which is
#' fixed to be the Theil-Sen Estimator.
#'
#' @param amax vector of AMAX values
#' @param years vector of timepoints corresponding to \code{amax}
#' @param init_params vector of initial parameter estimates \eqn{(loc_{int}, loc_{slope}, sca, sha)}
#'
#' @returns vector of parameters (loc, TSE, sha, sca)
#'
#' @export
parglo_mle_tse <- function (amax, years, init_params) {
  tse <- zyp::zyp.sen(amax~years)$coefficients[2]

  NY <- length(years)

  llhd <- function(par){
    xi    <- par[1] + years*tse
    alpha <- rep(par[2], NY)
    kappa <- rep(par[3], NY)

    if (alpha[1] < 0) {return(99999)}
    if (abs(kappa[1])>=1) {return(99999)}
    if (any((kappa > 0 & amax > xi + (alpha/kappa)) |
            (kappa < 0 & amax < xi + alpha/kappa))) {return(99999)}

    ll <- sum(sapply(1:length(amax),
                     function(i){
                       log(dglo(amax[i], xi[i], alpha[i], kappa[i]))}))
    return(-1*ll)
  }

  OP <- stats::optim(par=init_params,
                     fn=llhd,
                     hessian=FALSE,
                     method="BFGS",
                     control=list(maxit=4000))

  OP$par <- c(OP$par[1], tse, OP$par[2], OP$par[3])
  names(OP$par) <- c("xi", "tse", "alpha", "kappa")
  return(OP)
}


#' Percentiles of daily flow over specified periods
#'
#' This computes percentiles of flow given daily flow records,
#' values computed over each period as selected. Returned as time-series.
#'
#' @param daily two-column dataframe: date, flow
#' @param pc vector of percentiles of exceedence
#' @param period string (\code{month}, \code{year}, \code{decade}) indicating period
#'
#' @return data.frame with period, percentiles and period minimum and maximum.
#'
#' @export
periodPercentile <- function (daily, pc=c(0.5, 0.95), period="year") {
  #####
  # periodPercentile computes percentiles of flow given daily flow records,
  # values computed over each period as selected. Returned as time-series.
  #
  # daily     daily flow series (2 column: date, flow)
  # pc        vector of percentiles of exceedence
  # period    string ("month", "year", "decade") indicating period
  #
  # OUT
  # df        data.frame (period, flow_1, flow_2, ...) of percentiles
  #####

  exc <- c("MIN", "Q05%", "Q50%", "Q95%", "MAX")

  if(nrow(daily) < 2){
    stop("Daily flow data not found.")
  }
  colnames(daily) <- c("date", "flow")

  daily$year <- findHydrolYr(daily$date)$yr
  if(period == "month"){
    idv <- c("month", "year", "datelab")
    daily$month <- lubridate::month(daily$date)
    frm <- stats::as.formula(flow ~ month + year + datelab)
    daily$datelab <- as.Date(paste0(daily$year, "-", daily$month, "-01"))
  }else if(period == "decade"){
    idv <- c("decade", "datelab")
    daily$decade <- floor(daily$year/10)*10
    frm <- stats::as.formula(flow ~ decade + datelab)
    daily$datelab <- as.Date(paste0(daily$decade,"-01-01"))
  }else{
    idv <- c("year", "datelab")
    frm <- stats::as.formula(flow ~ year + datelab)
    daily$datelab <- as.Date(paste0(daily$year,"-01-01"))
  }

  quantA <- stats::aggregate(frm, data=daily,
                      FUN=function(v){stats::quantile(v, probs=pc)})
  quantB <- stats::aggregate(frm, data=daily,
                      FUN=range)

  dailysumm <- merge(quantA, quantB, by=idv)
  dailysumm <- do.call(data.frame,dailysumm)

  pnames <- paste0("P", formatC(floor(pc*100),
                                width = 2, format = "d", flag = "0"))

  colnames(dailysumm) <- c(idv, pnames, "PMIN", "PMAX")

  dailysumm
}

#' import data from API
#'
#' this
#'
#' @param st_no station NRFA identifier
#' @param start start year
#' @param end end year
#'
#' @export
importTriangle <- function(st_no, start, end){
  ### REPLACE WITH GET STATION FUNCTION
  api_call_path <- paste0("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?",
                         "format=nrfa-csv&data-type=amax-flow&station=",
                         st_no)

  # Extract AMAX
  tr_raw <- utils::read.csv(api_call_path, skip=21, col.names=c("Date","Flow"),
                     stringsAsFactors=FALSE)
  #print(am_raw)

  # if fewer than 2 rows of daily data (unlikely), treat station as missing.
  if(nrow(tr_raw) < 2){
   df <- data.frame(date=as.Date("2000-01-01"),
                       year=2000,
                       year0=0,
                       flow=0)

   # Default values
   stop(
     "Insufficient station data found. Please check the station ID.")

  }else{
   # Include Hydrological Year
   #print(findHydrolYr(as.Date(tr_raw$Date)))
   df <- data.frame(date= as.Date(tr_raw$Date),
                       year =findHydrolYr(tr_raw$Date)[,2],
                       flow = as.numeric(tr_raw$Flow))

   df <- subset(df, date >= start & date <= end)
   # year0 is shifted to make TSE more interpretable
   df$year0 <- df$year - floor(stats::median(df$year))
   if(length(unique(df$year))!=length(df$year)){
     stop("two events in same year.")
   }
  }
  if(nrow(df) < 27){
   stop("Multi-temporal analysis requires a window of at least 26 years.")
  }
  df
}

#' Triangle plotting of trends for all observed time windows
#'
#' This function takes the data.frame of amax flow, and returns a matrices of
#' Mann-Kendall test statistics and Theil-Sen estimates of trend for every choice
#' of start year and end year (with a minimum of 26 years of data)
#'
#' @section  Information about flags:
#' \enumerate{
#' \item for the years outside the range of data for each station give the value -100 \\
#' \item when there are less than 27 total years (NA included) give the value -200 \\
#' \item when there are: \itemize{
#'    \item more than 10% missing data or less than 27 actual data (NA excluded),\\
#'    \item or more than 2 missing values in the beginning of the subset,\\
#'    \item or more than 1 missing value in the end of the subset, then give the value -300 }\\
#' \item when end year is smaller than start year give the value -400
#' }
#'
#' @param ts a two column data.frame object of year and flow
#' @param startyr start year for triangle matrices
#' @param endyr end year for triangle matrices
#'
#' @return list of 2 matrices, MKZ values and TSE values.
#'
#' @export
triangleTrend <- function(ts, startyr, endyr){

  colnames(ts) <- c("year", "flow")
  ts$year0 <- ts$year - stats::median(ts$year)
  dataSub <- zoo::na.trim(ts)
  # create empty array
  ND <- nrow(dataSub)
  Mat <- matrix(NA, ND, ND)
  Mat_TSArel <- matrix(NA, ND, ND)
  dimnames(Mat) <-  list(End=dataSub$year, Start=dataSub$year)
  dimnames(Mat_TSArel) <- list(End=dataSub$year, Start=dataSub$year)

  All_combs <- expand.grid(End=rownames(Mat), Start=colnames(Mat))
  # loop through stations
  Used_Subsets <- vector('list', length = nrow(Mat)^2)
  # Get true data
  true.data <- dataSub$flow
  NonNAindex <- which(!is.na(true.data))
  firstNonNA <- min(NonNAindex)

  # relaxed criterion for start year, consistent with script 5
  firstNonNA <- ifelse(firstNonNA<=2,1,firstNonNA-2)
  lastNonNA <- max(NonNAindex)

  # relaxed criterion for end year, consistent with script 5
  if (lastNonNA<nrow(dataSub)){ lastNonNA <- lastNonNA + 1}

  for (yr_start in 1:dim(Mat)[2]){ # loop yr_start through starting years starts
    if (yr_start<firstNonNA){
      Mat[, yr_start] <- Mat_TSArel[, yr_start] <- -100
      Mat[yr_start, ] <- Mat_TSArel[yr_start, ] <- -100
    }else if (yr_start>lastNonNA){
      Mat[, yr_start] <- Mat_TSArel[, yr_start] <- -100
      Mat[yr_start, ] <- Mat_TSArel[yr_start, ] <- -100
    }else{
      for (yr_end in yr_start:dim(Mat)[1]){ # loop yr_end through ending years starts
        #print(paste("yr_end: ", yr_end))
        if(yr_end - yr_start<(27-1)){ # we need at least 27 years, meaning a difference of at least 26
          Mat[yr_end, yr_start] <- Mat_TSArel[yr_end, yr_start] <- -200
          #print(paste(yr_start, yr_end, "ping -200"))
        }else{
          used_data <- true.data[yr_start:yr_end] # subset used data
          NA_percent <- sum(is.na(used_data))/length(used_data) # 10% missing data criterion
          NonNAindex_subset <- which(!is.na(used_data)) # 2 years relaxation for start year
          firstNonNA_subset <- min(NonNAindex_subset) # 2 years relaxation for start year
          Length_used_data <- length(used_data[!is.na(used_data)]) # 27-year of actual data criterion
          lastNonNA_subset <- max(NonNAindex_subset) # 1 year relaxation for end year
          CriterionPass <- NA_percent<= 0.1 & Length_used_data>=27 &
            firstNonNA_subset<=3 & lastNonNA_subset>= (length(used_data) - 1)
          # combined criteria
          if(CriterionPass){
            Mat[yr_end, yr_start] <- relativeTheilSen(dataSub[yr_start:yr_end,3], used_data)
            # assign the used data to the correct position within the Used_Subsets list
            i_row <- which(All_combs$End==dataSub[yr_end,3] & All_combs$Start==dataSub[yr_start,3])
            Used_Subsets[[i_row]] <- used_data
          }else{
            Mat[yr_end, yr_start] <- Mat_TSArel[yr_end, yr_start] <- -300
          }
        }
      } # loop yr_end through ending years ends


    } # end criterion if the starting year falls out out the range of data of the station
  } # loop yr_start through starting years ends

  # read the list with the used data sets and keep only the ones with actual data
  Used_Subset <- Used_Subsets
  # data analysis for keeping only the unique combinations
  Used_combs <- which(unlist(lapply(Used_Subset, is.null))==T)
  Used_Subset[Used_combs] <- NA
  # remove start/end NAs are they are not used on MKZ test
  #browser()
  Used_Subset <- lapply(Used_Subset, zoo::na.trim)
  # keep only the location of the instances that have unique time-series (with lenght>0)
  Used_Subsets <- which(duplicated(Used_Subset)==F &
                          unlist(lapply(Used_Subset, function(x)length(x)))>0 )

  Mat[is.na(Mat)] <- -400 # all remaining NA's are places where end_year<start_year
  Mat_TSArel[is.na(Mat_TSArel)] <- -400

  return(list(MKZ=Mat, TSE=Mat_TSArel))
}

#' Triangle plotting of trends for all observed time windows
#'
#' This function uses the output from \code{triangleTrends} and plots the triangle
#' plot.
#'
#' @param MKZ matrix of Mann-Kendall Z-scores
#' @param ts time-series dataframe of year and flow
#'
#' @return a triangle plot with start year along the side, end year along the bottom.
#' Each gridsquare shows the Mann-Kendall statistic for that time period. Points which
#' are statistically significant at 95% are highlighted.
#'
#' @export
trianglePlot <- function(MKZ, ts){

  Sta.ts <- ts[, c('year', 'flow')]
  colnames(Sta.ts) <- c('Year', 'Q')
  Sta.ts <- zoo::na.trim(Sta.ts)
  Data_St <- reshape2::melt(MKZ, value.name="MKZ")
  colnames(Data_St)[3] <- 'MKZ'

  Pos_Values <- as.vector(MKZ[MKZ>0])
  Neg_Values <- as.vector(MKZ[MKZ>-100 & MKZ<0])

  Q_Pos <- stats::quantile(Pos_Values, c(1, 0.5, 0.25, 0))
  Q_Neg <- rev(-1*stats::quantile(-1*Neg_Values, c(1, 0.5, 0.25, 0)))

  # add a proper limit if no points go beyond +-1.96
  Q_Neg[4] <- ifelse(Q_Neg[4] < -1.96, Q_Neg[4], -2.0)
  Q_Pos[1] <- ifelse(Q_Pos[1] >  1.96, Q_Pos[1],  2.0)

  # adds a zero if there are only positive or negative values
  Q_Neg[1] <- ifelse(is.na(Q_Neg[1]), 0, Q_Neg[1])
  Q_Pos[4] <- ifelse(is.na(Q_Pos[4]), 0, Q_Pos[4])

  ALLNEG <- (length(Pos_Values)==0)
  ALLPOS <- (length(Neg_Values)==0)

  # Define breaks for classes for the values on the CUT function
  MAX_Q2.5 <- max(Neg_Values[Neg_Values< -1.96])
  if(is.infinite(MAX_Q2.5)){MAX_Q2.5 <- -1.96}
  # The -1.96 is replaced with the maximum value that is still lower -1.96
  # othwrwise if there is value exactly -1.96 it will be counted on the
  # Q100--1.96 and not on the -1.96-Q50 class
  Color_Breaks <- c(Q_Neg[4], -1.96, Q_Neg[3:1],
                    Q_Pos[4:2], 1.96, Q_Pos[1])
  Color_Breaks[c(3,4,7,8)] <-
    trunc(Color_Breaks[c(3,4,7,8)]*100)/100 + c(-2e-5, -1e-5, 1e-5, 2e-5)
  Color_Breaks[c(5,6)] <- trunc(Color_Breaks[c(5,6)]*1000)/1000
  Color_Breaks[1] <- floor(Color_Breaks[1]*100)/100
  Color_Breaks[10] <- ceiling(Color_Breaks[10]*100)/100
  Color_Breaks_Lab <- round(Color_Breaks, 2)

  Labels <- rep(NA, 10)
  Labels[1] <- 'NA'
  Labels[2] <- '< -1.96'
  Labels[3] <- paste0('[-1.96,', Color_Breaks_Lab[3], ')')
  Labels[4] <- paste0('[', Color_Breaks_Lab[3], ", ", Color_Breaks_Lab[4], ")")
  Labels[5] <- paste0('[', Color_Breaks_Lab[4], ',0)')
  Labels[6] <- '0'
  Labels[7] <- paste0('(0,', Color_Breaks_Lab[7], ']')
  Labels[8] <- paste0('(', Color_Breaks_Lab[7], ", ", Color_Breaks_Lab[8], "]")
  Labels[9] <- paste0('(', Color_Breaks_Lab[8], ", 1.96]")
  Labels[10] <- '> 1.96'

  Colors <- c('black', rev(RColorBrewer::brewer.pal(n = 8, "Reds")[c(2,4,6,8)]),
              'white', RColorBrewer::brewer.pal(n = 8, "Blues")[c(2,4,6,8)])

  if(ALLNEG){
    # if all negative, remove all positive colors and breaks.
    Color_Breaks <- Color_Breaks[1:6]
    Color_Breaks_Lab <- Color_Breaks_Lab[1:6]
    Colors <- Colors[1:6]
    Labels <- Labels[1:6]
  }else if(ALLPOS){
    # if all positive, remove all negative colors and breaks.
    Color_Breaks <- Color_Breaks[5:10]
    Color_Breaks_Lab <- Color_Breaks_Lab[5:10]
    Colors <- Colors[c(1,6:10)]
    Labels <- Labels[c(1,6:10)]
  }

  AllData <- reshape2::melt(MKZ)
  AllData <- AllData[AllData$value == -300 | AllData$value > -100, ]
  AllData$value[AllData$value == -300] <- NA
  AllData$Discrete <- cut(AllData$value, breaks=Color_Breaks,
                          include.lowest = T, dig.lab = 7)

  Levels <- levels(AllData$Discrete)
  Levels <- c('NA', Levels) # Add the 'NA' as level

  # Remove any (a,a] type intervals from the plot
  if(ALLPOS | ALLNEG){
    if(Color_Breaks_Lab[3] == Color_Breaks_Lab[4]){
      Color_Breaks_Lab <- Color_Breaks_Lab[-3]
      Color_Breaks <- Color_Breaks[-3]
      Colors <- Colors[-4]
      Levels <- Levels[-4]
      Labels <- Labels[-4]
    }
  }else{
    if(Color_Breaks_Lab[7] == Color_Breaks_Lab[8]){
      Color_Breaks_Lab <- Color_Breaks_Lab[-7]
      Color_Breaks <- Color_Breaks[-7]
      Colors <- Colors[-8]
      Levels <- Levels[-8]
      Labels <- Labels[-8]
    }
    if(Color_Breaks_Lab[3] == Color_Breaks_Lab[4]){
      Color_Breaks_Lab <- Color_Breaks_Lab[-3]
      Color_Breaks <- Color_Breaks[-3]
      Colors <- Colors[-4]
      Levels <- Levels[-4]
      Labels <- Labels[-4]
    }
  }

  Data_St$Discrete <- cut(Data_St$MKZ, breaks=Color_Breaks,
                          include.lowest = T, dig.lab = 7)
  Data_St$Discrete <- factor(Data_St$Discrete, levels = Levels, ordered = T)
  Data_St$Discrete[is.na(Data_St$Discrete)] <- 'NA'

  Data_St$Significance <- 1*(Data_St$MKZ > -100 & abs(Data_St$MKZ) > 1.96)
  Data_St$Significance <- factor(Data_St$Significance)
  # convert to factor for better use at ggplot

  min_st  <- range(Data_St$Start[which(Data_St$MKZ > -99)])
  min_end <- range(Data_St$End[which(Data_St$MKZ > -99)])

  Data_St <-  Data_St[Data_St$Start >= min_st[1]  &
                        Data_St$Start <= min_st[2]  &
                        Data_St$End   >= min_end[1] &
                        Data_St$End   <= min_end[2] ,]

  pb_st <- function(x){pretty(x, min(diff(min_st)+1, 7))}
  pb_end <- function(x){pretty(x, min(diff(min_end)+1, 7))}

  Main_Actual <- ggplot2::ggplot(data = Data_St) +
    ggplot2::geom_tile(ggplot2::aes(x = Start, y = End, fill = Discrete)) +
    ggplot2::geom_point(
      data = Data_St[Data_St$Significance == 1,],
      size = 40 / diff(range(Data_St$Start)),
      # size of signif. indicator inversely proportional to number of years
      ggplot2::aes(x = Start, y = End, color = Significance)
    ) +
    ggplot2::labs(x = 'Start year', y = 'End year') +
    ggplot2::scale_fill_manual(
      name = 'MKZ',
      values = Colors,
      limits = Levels,
      labels = Labels
    ) +
    ggplot2::scale_color_manual(values = 'green',
                       limits = 1,
                       labels = 'significant \n(5% level)') +
    ggplot2::guides(
      fill = ggplot2::guide_legend('MKZ', reverse = T, order = 1),
      color = ggplot2::guide_legend(
        title.theme = ggplot2::element_blank(),
        order = 2,
        override.aes = list(size = 3) ) ) +
    ggplot2::theme(
      legend.box.background =
        ggplot2::element_rect(fill = 'grey95', colour = 'grey95'),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)
    ) +
    ggplot2::coord_fixed(ratio=1)+
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = pb_end) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = pb_st) +
    ggplot2::theme(legend.text=ggplot2::element_text(size=11))
  Main_Actual
}


#' Plotting of AMAX series with trend lines
#'
#' Uses a time-series and fitted parameters to add lines indicating estimated
#' magnitudes of events of given return periods.
#'
#' @param x time-series dataframe of year and flow
#' @param param vector of GLO parameters (loc, sca, sha) for stationary distributions and (loc_int, loc_slope, sca, sha)
#' for non-stationary distributions
#' @param start start year of plot
#' @param end end year of plot
#' @param rp vector of return periods (in years for AMAX data)
#' @param NST if true, plots non-stationary return period lines.
#'
#' @return a plot with the time-series and lines indicating magnitude of events with
#' given return periods over time.
#'
#' @export
amaxPlot <- function(x, param, start, end, rp=2, NST=FALSE){
  if(length(rp)<1){
    stop("provide return periods.")
  }
  colnames(x) <- c("date", "flow")
  x$year0 <- x$year - stats::median(x$year)
  qq <- 1-(1/rp)
  graphics::par(mar=c(3,4,0.4,0.4), mgp=c(2,1,0))
  vird <- viridis::viridis(length(rp))
  graphics::plot(x$date, x$flow,
       xlim=c(start, end),
       ylim=c(0, 2*max(x$flow)),
       xlab="Date",
       ylab=expression(paste("Annual Maximum Flow (", m^3~s^-1, ")")))
  if(length(rp)>0){
    for(i in 1:length(rp)){
      if(NST){
        qm <- param[1] + param[2]*(x$year0) +
          (param[3]/param[4])*(1 - ((1-qq[i])/(qq[i]))^param[4])
      }else{
        qm <- rep(qglo(qq[i], loc=param[1], scale=param[2], sh=param[3]),
                  length(x$date))
      }
      graphics::lines(x$date, qm, col=vird[i], lwd=1.5, )
      graphics::text(x$date[length(x$date)]+10,
           qm[length(qm)],
           label=rp[i], pos=4)
    }
  }
  if(NST){
    pval <- signif(KendallZScore(x$flow), 4)
    sigstar <- ifelse(abs(pval)>1.96, "*", "")
    graphics::text(start, 2*max(x$flow),
         paste0("MKZs = ", pval, sigstar),
         pos=4, cex=1.5)
  }
  graphics::legend("topright", legend=rp,
                   col=vird, lwd=1,
                   title="Return Periods")
}
