# Adam Griffin, 2018-12-12
# 2018-12-17: List of helpful locations of data and commands
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# site_fetch <- jsonlite::fromJSON(
#   txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/stations",
# simplifyDataFrame=T)
# lookup_fetch <- jsonlite::fromJSON(
#   txt="https://gateway-staging.ceh.ac.uk/hydrology-ukscape/lookup",
#   simplifyDataFrame=T)

fetchGDF <- jsonlite::fromJSON(
     txt=paste0("https://gateway-staging.ceh.ac.uk/hydrology-ukscape/stations/",
                "EA/gdf/723a8fc4-908b-4430-91c7-9990be86540a/1900-01-01/1901-01-01"),
     simplifyDataFrame=T)

# /stations
# [[1]] organisation = c("NRFA", "EA", "SEPA", "COSMOS")
# [[2]] metadata [[1]] name, [[2]] notes, [[3]] datatypes (tables)
# [[3]] data (table for each org)
