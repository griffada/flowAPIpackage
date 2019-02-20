#' Import time series from river flow API.
#'
#' Using the river flow/rainfall API, time series can be extracted, either
#' selecting single dates, periods of record, or entire records for single or
#' multiple sites. Metadata can also be returned for stations in the dataset.
#' All data must be of the same type from the same organisation over the
#' same period.
#'
#' @param ids identifier for stations (not EA refs)
#' @param dat string indicating datatype, as written in metadata.
#' @param org organisation from whom the data is obtained.
#' @param startDate string of the form \code{YYYY-MM-DD} to indicate start of
#'     period desired, or single date. Whole record given if no startDate
#'     provided.
#' @param endDate string of the form \code{YYYY-MM-DD} to indicate end of
#'      period desired. If no startDate provided, this is ignored.
#' @param metadata if \code{TRUE}, returns metadata for each station selected.
#' @param datetime if \code{TRUE}, converts datetime column into POSIXlt.
#'
#' @return a dataframe containing the dates and magnitudes of the selected
#'     data. If multiple stations selected, the dataframes are contained in a
#'     named list. If metadata is true, each station will consist of a list
#'     containing \code{detail} and \code{data}.
#'     If not found, returns NA for each such station.
#'
#' @examples
#' \dontrun{
#' import_ts(ids=c("SX67F051", "SS50F007"), org="EA", dat="gdf",
#'     startDate="2017-01-01", endDate="2017-02-01")
#' import_ts(ids="SX67F051", org="EA", dat="gdf", metadata=T)
#' }
#'
#' @export import_ts
import_ts <- function(ids, dat, org = c("NRFA", "EA", "SEPA", "COSMOS"),
                      startDate = NULL, endDate = NULL, metadata=FALSE,
                      datetime = TRUE){

  ids <- as.character(ids)  #ids for respective dataset, not refs
  org <- match.arg(org)
  if (!is.null(startDate)) {
    startDate <- lubridate::as_datetime(x=startDate,
                  format=lubridate::guess_formats(startDate, c("dmy", "ymd")),
                  tz="UTC")[1]
  }
  if (!is.null(endDate)) {
   endDate <- lubridate::as_datetime(x=endDate,
                format=lubridate::guess_formats(endDate, c("dmy", "ymd")),
                tz="UTC")[1]
  }
  ## should convert likely date strings/objects to date objects

  li <- length(ids)

  if (li == 0) { stop("Enter valid id for station(s).") }

  stationListId <- stationList(org)$id

  if (all(!(ids %in% stationListId))) {
    # check data available for any stations
    stop("No supplied stations available in selected list.")
  }

  ts <- ts_fetch_internal(ids, org, dat, startDate, endDate)
  # if(!(names(ts)[1] %in% c("detail", "data"))){
  names(ts) <- ids
  # }

  # if(li == 1){
  #   ts <- list(ts)
  # }

  #ts <- lapply(ts, function(y){y$data <- y$data[,c('datetime','value')]})

  if (datetime) {
    ts <- lapply(ts, function(y){y$data <- ts_reformat(y$data);y})
  }

  if (!metadata) {
    ts <- lapply(ts, function(y){y['data',drop=F]})
  }

  #print(paste("li = ", li))

  if (li == 1) {
    ts <- ts[[1]]
  }
  # }

  return(ts)
}




#' Reformats a time series to have datetime objects.
#'
#' Converts a data.frame with strings for datetimes into one with POSIXlt date
#' objects.
#'
#' @param ts time series data.frame object of two columns: datetime
#' (string of datetimes in form \code{YYYY-MM-DDTHH:MM:SSZ}) and
#' data (numeric).

#' @return data.frame with replaced datetime column containing equivalent
#'     POSIXlt objects.
#'
#' @export ts_reformat
ts_reformat <- function(ts){

  if (all(is.na(ts))) return(ts)

  if (is.data.frame(ts)) {

    ts$datetime <- lubridate::as_datetime(ts$datetime,
                                          format="%Y-%m-%dT%H:%M:%OSZ",
                                          tz="UTC")

  }else{
    #need to reconstruct the data.frame
    ts <- lapply(ts, function(l){
      l$datetime <- lubridate::as_datetime(l$datetime,
                                           format="%Y-%m-%dT%H:%M:%OSZ",
                                           tz="UTC")
      l
    })
  }
  return(ts)
}


#' Import metadata from river flow API.
#'
#' Using the river flow/rainfall API, station information can be extracted for
#' single or multiple sites. All data must be of the same type from the same
#' organisation.
#'
#' @param ids identifier for stations (not EA refs)
#' @param dat string indicating datatype, as written in metadata.
#' @param org organisation from whom the data is obtained.
#'
#' @return a list containing:
#' \itemize{
#' \item id - measuring authority station identifier
#' \item ref - API reference string
#' \item name - station name
#' \item organisation
#' \item station aliases under different organisations
#' \item datatype - list of descriptors of data
#' \item startDate - character string of first record
#' \item dataUrl - string of URL to obtain data from API directly.
#' }
#'      If not found, returns NA for each such station.
#'
#' @examples
#' \dontrun{
#' import_metadata(ids=c("SX67F051", "SS50F007"), org="EA", dat="gdf")
#' import_metadata(ids="SX67F051", org="EA", dat="gdf")
#' }
#'
#' @export import_metadata
import_metadata <- function(ids, dat, org = c("NRFA", "EA", "SEPA", "COSMOS")){

  ids <- as.character(ids)  #ids for respective dataset, not refs
  org <- match.arg(org)
  li <- length(ids)

  if (li == 0) { stop("Enter valid id for station(s).") }

  stationListId <- stationList(org)$id

  if (all(!(ids %in% stationListId))) {
    # check data available for any stations
    stop("No supplied stations available in selected list.")
  }

  ts <- ts_fetch_internal(ids, org, dat, startDate=NULL, endDate=NULL)
  names(ts) <- ids
  ts <- lapply(ts, function(y){y['detail',drop=F]})

  if (li == 1) ts <- ts[[1]]

  return(ts)

}










# Import time series directly from river flow API.
#
# Using the river flow/rainfall API, time series can be extracted, either
# selecting single dates, periods of record, or entire records for single
# or multiple sites.
# This function directly calls the API.
#
# @param ids identifier for stations (not EA refs)
# @param dat string indicating datatype, as written in metadata.
# @param org organisation from whom the data is obtained.
# @param startDate POSIXlt object to indicate start of period desired, or
#  single date. Whole record given if no startDate provided.
# @param endDate POSIXlt object to indicate end of period desired. If no
#  startDate provided, this is ignored.
#
# @return a dataframe containing the dates and magnitudes of the selected
#     data.
#     If not found, returns NA for each such station.
#
# @examples
# \dontrun{
# startDate <- lubridate::as_datetime("1901-01-01")
# endDate <- lubridate::as_datetime("1901-02-01")
# ts_fetch_internal(ids=c("SX67F051", "SS50F007"), org="EA", dat="gdf",
#     startDate=startDate, endDate=endDate)
# }
#
ts_fetch_internal <- function(ids, org, dat, startDate=NULL, endDate=NULL){
# fetches relevant time series and metadata information from API

  if (org == "EA") {
    refs <- idToRef(ids)
  }else{
    refs <- ids
  }

  # generate url to relevant API page
  txt <- paste0("https://gateway-staging.ceh.ac.uk/hydrology-ukscape/",
                "stations/",org,"/",dat,"/",refs)
  if (!is.null(startDate)) {
    txt <- paste0(txt,"/",format(startDate, "%Y-%m%-%d"))
    # if one date provided only gives that date
    if (!is.null(endDate)) {
      txt <- paste0(txt,"/",format(endDate, "%Y-%m%-%d"))
    }
  }


  txt <- as.list(txt)

  # checks that address works
  accesstest <- sapply(txt, function(y){
    class(try(jsonlite::fromJSON(txt=y, simplifyDataFrame=T),
              silent=T)) != "try-error"
  })

  if (sum(!accesstest)>0) {
    message(paste0("Not possible to access ", dat, " data for stations ",
                   paste(ids[!accesstest], sep=", "), "."))
  }

  ts_fetch <- vector("list", length(ids))

  # get data from successfully tested stations
  ts_fetch[accesstest] <- lapply(txt[accesstest],
                          jsonlite::fromJSON, simplifyDataFrame=T)

  ts_fetch[!accesstest] <- NA

  # check for wrong period of time
  datatest <- sapply(ts_fetch,
                     function(y){is.list(y) && is.data.frame(y$data)})

  if (sum(!datatest & accesstest) > 0) {
    message(paste0("No ", dat, " data for stations ",
                   paste(ids[!datatest & accesstest], sep=", "),
                   ". Check period selected."))
  }

  # make all stations have same format
  #ts_fetch[!accesstest | !datatest] <- list(list("detail"=NULL, "data"=NULL))
  ts_fetch <- replace(ts_fetch,
                      which(!accesstest | !datatest),
                      list(list("detail"=NA, "data"=NA)))

  #if (length(ts_fetch) == 1) ts_fetch <- ts_fetch[[1]]

  return(ts_fetch)
}
