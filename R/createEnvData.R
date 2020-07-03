# Derived from E. Gurarie's [ABoVE](https://github.com/ABoVE-AotM/above), with modifications
# format to env data required timestamp
format_time <- function(time_vec) {
  # time format: 2007-04-20 0:00:00.000 in UTC. ?format.POSIXct
  time_format <- "%Y-%m-%d %H:%M:%OS3"
  # format(Sys.time(), time_format)
  # with_tz(Sys.time(), tzone = "UTC")
  format(lubridate::with_tz(time_vec, tzone = "UTC"), time_format)
}

##' Convert data frame to Movebank Env-Data request file
##'
##' Takes a data frame of latitudes, longitudes and times and generates the strictly formatted data frame needed to upload to Env-Data to obtain covariates for movebank.
##'
##' @param df data.frame in [movebank format](https://www.movebank.org/node/13):
##'   longitude and latitude in WGS84, timestamp in POSIXct or ymd_hms format,
##'   and height above ellipsoid (in m). If no timezone information exist, it
##'   will be interpreted as UTC time. If no height above ellipsoid provided it
##'   will be assigned as NA.
##' @param existing_names column name vector in order of timestamp, latitude,
##'   longitude, and height above ellipsoid (the last can be omitted).
##' @return data frame with correct formatting.
##' @examples
##' library(ctmm)
##' library(data.table)
##' data(buffalo)
##' df <- data.frame(buffalo[[1]])
##' existing_names <- c("timestamp", "longitude", "latitude")
##' dt <- convert_env_dataframe(df, existing_names)
##'
##' @export

convert_env_dataframe <- function(df, existing_names){
  dt <- data.table(df)
  standard_names <- c("timestamp", "location-lat", "location-long",
                      "height-above-ellipsoid")
  # not checking every input error here
  if (length(existing_names) == 3) {
    warning("Only 3 column names provided, assigning height-above-ellipsoid to NA")
    old_names <- c(existing_names, "height-above-ellipsoid")
    dt[, `height-above-ellipsoid` := ""]
  }
  setnames(dt, old_names, standard_names)
  # expecting timestamp to be POSIXct with right timezone
  dt[, timestamp_original := timestamp]  # as backup, always operate on timestamp
  # if being common string, try ymdhms to parse it. otherwise timestamp should be POSIXct already
  if (!lubridate::is.POSIXct(dt$timestamp)) {
    warning("Timestamp is not POSIXct, trying to parse with ymdhms format")
    dt[, timestamp := lubridate::ymd_hms(timestamp)]
    if (any(is.na(dt$timestamp))) {
      warning("Timestamp parsing failed on\n")
      print(dt[is.na(timestamp)])
    }
  }
  dt[, timestamp := format_time(timestamp)]
}


##' Create gridded data for querying Movebank Env-Data
##'
##' Takes a vector of latitudes and longitudes (typically evenly spaced) and a vector of times and generates the strictly formatted data frame needed to upload to Env-Data to obtain covariates for movebank. The location vector can be single location, and time vector can be single time.
##'
##' @param {lons, lats} vectors of longitude and latitude for the desired grid.
##'   height.above.ellipsoid will be assigned as NA.
##' @param times vector of POSIXct times. See example code for how to generate them.
##' @return data frame with correct formatting
##' @examples
##'  library(lubridate)
##'  lats <- seq(38.8, 39.0, length = 40)
##'  lons <- seq(-77.12, -76.91, length = 40)
##'  # times need to be POSIXct, not Date
##'  start <- ymd_hms("2014-01-01 0:00:00")
##'  finish <- ymd_hms("2014-12-31 0:00:00")
##'  timestep <- ddays(20)
##'  times <- seq(start, finish, by = timestep)
##'  test_grid <- create_env_data_grid(lats, lons, times)
##'
##' @export

create_env_data_grid <- function(lats, lons, times){
  times_formated <- format_time(times)
  latlontime <- expand.grid(lats, lons, times_formated)
  data.frame(timestamp = latlontime$Var3,
             'location.lat' = latlontime$Var1,
             'location.long' = latlontime$Var2,
             'height.above.ellipsoid' = "")

}

