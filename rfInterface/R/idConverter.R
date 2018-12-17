# Adam Griffin 2018-12-17
# Functions to find ids and names of stations and convert if needed.


#' Convert between different station identifiers.
#'
#' Given a station identifier, and its source (either NRFA or EA/SEPA), this function
#' provides the identifier designated by the other organisation. idConverter
#' assumes that any given station is uniquely monitored by EA or SEPA, not both.
#'
#' @param num vector of numbers or strings containing identifier(s) of station
#' (all should come from same source)
#' @param source string indicating source of provided identifier(s), not target.
#' @return the identifier(s) from the other data source not selected in 'source'.
#' If not found, returns NA.
#' @examples
#' idConverter(39071, "NRFA")  # NRFA to EA/SEPA
#' idConverter("0130TH", "EA")  # EA to NRFA
#' @export idConverter
idConverter <- function(num, source=c("NRFA", "EA", "SEPA")){
  num <- as.character(num)
  source <- match.arg(source)
  inCol <- switch(source,
                  NRFA = "NRFA_STATION",
                  EA =,
                  SEPA = "id")
  outCol <- switch(source,
                   NRFA = "id",
                   EA =,
                   SEPA = "NRFA_STATION")

  lookup_fetch <- jsonlite::fromJSON(
    txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/lookup",
    simplifyDataFrame=T)

  target <- match(num, lookup_fetch[,inCol])  # repeated which function
  return(lookup_fetch[target,outCol])
}

#' Gives a list of stations from the relevant organisation.
#'
#' Returns a dataframe of NRFA-associated stations from EA or SEPA, or both.
#' Columns include NRFA station number, latitude, longitude, id,  JSON reference,
#' and corresponding identifiers under other organisations.
#' @param org organisation to obtain list of stations from.
#' @return dataframe containing station names, ids and other important information.
#' @examples
#' stationList("NRFA")
#' @export stationList
stationList <- function(org = c("EA", "NRFA", "SEPA")){
  org <- match.arg(org)
  listno <- switch(org,
                   "EA" = 2,
                   "NRFA" = 1,
                   "SEPA" = 3)
  station_fetch <- jsonlite::fromJSON(
    txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/stations",
    simplifyDataFrame=T)

  jsonlite::flatten(station_fetch$data[[listno]])
}

#' Obtain reference for river flow API page.
#'
#' For EA stations for which daily flow is provided, this provides the reference
#' string for the API, given the EA identifier.
#'
#' @param id EA station number.
#' @return string to find flow from API
#' @examples
#' \dontrun{
#' idToRef("SS50F007")
#' }
#' @export idToRef
idToRef <- function(id){
  id <- as.character(id)
  station_fetch <- jsonlite::fromJSON(
    txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/stations",
    simplifyDataFrame=T)
  station_fetch <- station_fetch$data[[2]]
  if(!(id %in% station_fetch$id)){
    stop("Station ID not found in EA table.")
  }else{
    return(station_fetch$ref[which(station_fetch$id == id)])
  }
}
