# Adam Griffin 2018-12-17
# Functions to find ids and names of stations and convert if needed.


#' Convert between different station identifiers.
#'
#' Given a station identifier, and its source (either NRFA or EA/SEPA), this function
#' provides the identifier designated by the other organisation. idConverter
#' assumes that any given station is uniquely monitored by EA or SEPA, not both.
#'
#' @param id vector of numbers or strings containing identifier(s) of station
#'     (all should come from same org)
#' @param org string indicating source of provided identifier(s), not target.
#'
#' @return the identifier(s) from the other data providers not selected in 'org'.
#'     If not found, returns NA.
#'
#' @examples
#' \dontrun{
#' idConverter(39071, "NRFA")  # NRFA to EA/SEPA
#' idConverter("0130TH", "EA")  # EA to NRFA
#' }
#'
#' @export idConverter
idConverter <- function(id, org=c("NRFA", "EA", "SEPA")){

  id <- as.character(id)
  org <- match.arg(org)
  inCol <- switch(org,
                  NRFA = "NRFA_STATION",
                  EA =,
                  SEPA = "STATION_REFERENCE")
  outCol <- switch(org,
                   NRFA = "STATION_REFERENCE",
                   EA =,
                   SEPA = "NRFA_STATION")

  lookup_fetch <- lookup_fetch_internal()

  target <- match(id, lookup_fetch[,inCol])  # repeated which function
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
  station_fetch <- station_fetch_internal()$data[[2]]
  if(all(!(id %in% station_fetch$id))){
    stop("no Station IDs found.")
  }else{
    return(station_fetch$ref[match(id,station_fetch$id)])
  }
}

#### INTERNAL FUNCTIONS ####

lookup_fetch_internal <- function(){
  # internal function to call the lookup table from API
  # if(exists(lookup_fetch)){
  #   return(lookup_fetch)
  # }else{
    jsonlite::fromJSON(
      txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/lookup",
      simplifyDataFrame=T)
  #}
}

station_fetch_internal <- function(){
# internal function to call station list tabel from API
  # if(exists(station_fetch)){
  #   return(station_fetch)
  # }else{
    jsonlite::fromJSON(
      txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/stations",
      simplifyDataFrame=T)
  #}
}
