% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfInterface-package.R
\docType{package}
\name{rfInterface}
\alias{rfInterface}
\alias{rfInterface-package}
\title{Functions for Using UK Riverflow and Rainfall API}
\description{
rfInterface: Functions for Using UK Riverflow and Rainfall API
}
\details{
rfInterface provides functions to easily access EA and NRFA river
 flow data and SEPA rainfall data, with utility functions to convert station
 identifiers and location grid references. Preview of data at
 \url{https://gateway-staging.ceh.ac.uk/hydrology-ukscape}.

 The Environment Agency (EA) data is currently restricted to Devon/Cornwall
 riverflow data starting from 2017-01-01. EA stations have a station identifier
 (either numeric or alphanumeric, approximately 8 characters), and a reference
 string which the API uses (alphanumeric with dashes, approximately 36
 characters).

 The Scottish Environment Protection Agency (SEPA) data is currently
 restricted to rainfall in Scotland from 2018-02-13. Rainfall is presented as
 daily catchment total rainfall for given catchments corresponding to
 riverflow gauging stations. SEPA stations have a 6-7 digit station number,
 which is also its API reference string.

 The National River Flow Archive (NRFA) provides gauged daily river flow
 across the UK. Currently river flow data is restricted to the period
 2017-01-01 to 2018-09-01. NRFA stations have a 6-7 digit station number,
 which is also its API reference string.
}
