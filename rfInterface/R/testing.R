# debug(import_ts)
# debug(ts_reformat)

# idC <- idConverter()
# ts1 <- import_ts(ids=c("SX67F051", "SS50F007"), org="EA", dat="gdf",
#                         startDate="2017-01-01", endDate="2017-02-01")
# ts2 <- import_ts(ids="SX67F051", org="EA", dat="gdf")
# ts3 <- import_ts(ids="XX67F051", org="EA", dat="gdf")
# ts4 <- import_ts(ids="XX67F051", org="NRFA", dat="gdf")
# ts5 <- import_ts(ids=c("SX67F051"), org="EA", dat="gdf",
#                         startDate="1901-01-01", endDate="1901-02-01")
# ts6 <- import_ts(ids=c("XX67F051", "SS50F007"), org="EA", dat="gdf",
#                          startDate="2017-01-01", endDate="2017-02-01")
# ts7 <- import_ts(ids="SX67F051", org="EA", dat="cdr")
# ts8 <- import_ts(ids="SX67F051", org="EA", dat="gdf", metadata=T)
#
# debug(ts_fetch_internal)
# ts1 <- ts_fetch_internal(ids=c("SX67F051", "SS50F007"), org="EA", dat="gdf",
#                         startDate="2017-01-01", endDate="2017-02-01")
# ts2 <- ts_fetch_internal(ids="SX67F051", org="EA", dat="gdf")
# ts3 <- ts_fetch_internal(ids="XX67F051", org="EA", dat="gdf")
# ts4 <- ts_fetch_internal(ids="XX67F051", org="NRFA", dat="gdf")
# ts5 <- ts_fetch_internal(ids=c("SX67F051"), org="EA", dat="gdf",
#                         startDate=lubridate::as_datetime("1901-01-01"),
#                         endDate=lubridate::as_datetime("1901-02-01"))
# ts6 <- ts_fetch_internal(ids=c("XX67F051", "SS50F007"), org="EA", dat="gdf",
#                         startDate=lubridate::as_date("2017-01-01"),
#                         endDate=lubridate::as_date("2017-02-01"))
# ts7 <- ts_fetch_internal(ids="SX67F051", org="EA", dat="cdr")
