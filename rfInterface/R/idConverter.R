# Adam Griffin 2018-12-17
# Functions to find ids and names of stations and convert if needed.


#' Convert between different station identifiers.
#'
#' Given a station identifier, and its source (either NRFA or EA/SEPA), this function
#' provides the identifier designated by the other organisation. idConverter
#' assumes that any given station is uniquely monitored by EA or SEPA, not both.
#'
#' @param num vector of numbers or strings containing identifier(s) of station
#'     (all should come from same source)
#' @param source string indicating source of provided identifier(s), not target.
#'
#' @return the identifier(s) from the other data source not selected in 'source'.
#'     If not found, returns NA.
#'
#' @examples
#' \dontrun{
#' idConverter(39071, "NRFA")  # NRFA to EA/SEPA
#' idConverter("0130TH", "EA")  # EA to NRFA
#' }
#'
#' @export idConverter
idConverter <- function(num, source=c("NRFA", "EA", "SEPA")){

  num <- as.character(num)
  source <- match.arg(source)
  inCol <- switch(source,
                  NRFA = "NRFA_STATION",
                  EA =,
                  SEPA = "STATION_REFERENCE")
  outCol <- switch(source,
                   NRFA = "STATION_REFERENCE",
                   EA =,
                   SEPA = "NRFA_STATION")

  lookup_fetch <- lookup_fetch_internal()

  target <- match(num, lookup_fetch[,inCol])  # repeated which function
  return(lookup_fetch[target,outCol])
}





#' Gives a list of stations from the relevant organisation.
#'
#' Returns a dataframe of stations from a selected organisation.
#' Columns include NRFA station number, latitude, longitude, id, JSON
#' reference, and corresponding identifiers under other organisations.
#'
#' @param org organisation to obtain list of stations from.
#'
#' @return dataframe containing station names, ids and other important information.
#'
#' @examples
#' \dontrun{
#' stationList("NRFA")
#' }
#'
#' @export stationList
stationList <- function(org = c("EA", "NRFA", "SEPA", "COSMOS")){

  org <- match.arg(org)
  listno <- switch(org,
                   "EA" = 2,
                   "NRFA" = 1,
                   "SEPA" = 3,
                   "COSMOS" = 4)
  station_fetch <- station_fetch_internal()

  jsonlite::flatten(station_fetch$data[[listno]])
}





#' Obtain station reference for river flow API.
#'
#' For EA/SEPA/COSMOS stations for which data is provided, this provides the
#' reference string for the API, given the
#'  identifier.
#'
#' @param id EA/SEPA/COSMOS station id.
#'
#' @return ref string to request flow from API
#'
#' @examples
#' \dontrun{
#' idToRef("SS50F007")
#' }
#'
#' @export idToRef
idToRef <- function(id){
  id <- as.character(id)
  station_fetch <- station_fetch_internal()
  station_fetch <- station_fetch$data[[2]]
  if(all(!(id %in% station_fetch$id))){
    stop("no Station IDs found.")
  }else{
    return(station_fetch$ref[match(id,station_fetch$id)])
  }
}

#### INTERNAL FUNCTIONS ####

lookup_fetch_internal <- function(){
# internal function to call the lookup table from API
  jsonlite::fromJSON(
    txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/lookup",
    simplifyDataFrame=T)
}

station_fetch_internal <- function(){
# internal function to call station list tabel from API
  jsonlite::fromJSON(
    txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/stations",
    simplifyDataFrame=T)
}
