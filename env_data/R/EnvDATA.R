# functions relate to Movebank Env-DATA service
# common settings ----
base_url <- "http://www.bioinfo.mpg.de/orn-gateway"

# Derived from E. Gurarie's [ABoVE](https://github.com/ABoVE-AotM/above), with modifications
# prepare request ----
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
##' dt <- to_env_dataframe(df, existing_names)
##'
##' @export

to_env_dataframe <- function(df, existing_names){
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
##'  test_grid <- create_data_grid(lats, lons, times)
##'
##' @export

create_data_grid <- function(lats, lons, times){
  times_formated <- format_time(times)
  latlontime <- expand.grid(lats, lons, times_formated)
  data.frame(timestamp = latlontime$Var3,
             'location.lat' = latlontime$Var1,
             'location.long' = latlontime$Var2,
             'height.above.ellipsoid' = "")

}
# requesting ----
#' Creates Env-DATA XML query
#'
#' @param request_name Name for request
#' @param type_name Data type requested (e.g. "modis-land/MOD13A1.005"). Go to \link{http://www.bioinfo.mpg.de/orn-gateway/services2.jsp} to see list. To be entered exactly as seen with no spaces.
#' @param interpolation_type One of "bilinear", "inverse-distance-weighted", "nearest-neighbor".  See \link{https://www.movebank.org/node/6400} for details
#' @param variable_name Variable name (e.g. "500m 16 days EVI").  See list at link above.
#' @param dist_func_spatial function used for distance computation ... "geodetic" default generally fine.
#' @param email email address for notification
#' @param savefile whether or not to save a file (for actual use, must be TRUE)
#' @param fileout a file name - will be prompted if left empty
#' @return a \code\link{XMLNode} object which enters the request infromation onto the online EnvData form.
#' @export
#' @seealso createEnvDataGrid, uploadEnvDataRequest
#' @examples
#' request_name <- "EVI_request"
#' type_name <- "modis-land/MOD13A1.005"
#' variable_name <- "500m 16 days EVI"
#' interpolation_type <- "bilinear"
#' create_request("MyRequest", type_name, variable_name, interpolation_type, save_to_path = "testfile.xml")

create_request <- function(request_name = "MyRequest",
                                  type_name, variable_name,
                                  interpolation_type,
                                  dist_func_spatial="geodetic",
                                  email = "me@my.place",
                                  save_to_path) {

  variable_label <- paste(type_name, "/", variable_name, sep = "")

  a <- xmlNode("Properties", xmlNode("AnnotationRequestElementProperty", attrs=list(name="interpolation-type", value=interpolation_type)),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="type-name", value=type_name)),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="type", value="simple")),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="distance-function-spatial", value=dist_func_spatial)),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="variable-names", value=variable_name)))
  b <- xmlNode("AnnotationRequestElement", attrs=c(variableLabel=variable_label), a)
  c <- xmlNode("Elements", b)

  Request_xml <- xmlNode("AnnotationRequest", attrs=list(annotationType="track2", name=request_name, notificationEmail=email), c)
  saveXML(Request_xml, file = save_to_path)
  # if(savefile){
  #   if(is.null(fileout))
  #     fileout <- readline(prompt="Please provide filename for XML request: ")
  #   cat("Saving xml request to", fileout, "\n")
  #   cat(saveXML(Request_xml), file = fileout)
  # }
  # else return(Request_xml)
}

#' Upload an Env-Data request for Annotation
#'
#' @param csv_path Location and name of \code{.csv} file to annotate, either a MoveBank track or file generated by \code{\link{createEnvDataGrid}}
#' @param xml_path Location and name of \code{.xml} file that contains the request instructions - almost certainly generated by \code{\link{createEnvDataRequest}}
#' @param {login,password} login and password for EnvData (NOT the same as MoveBank login and password)
#' @param link_path name of link to access status and final annotated data
#'
#' @details The function will fill out the data annotation form on the EnvData annotation tool at \link{http://www.bioinfo.mpg.de/orn-gateway/services2.jsp}.  It will also create a link to the status report of the annotation request, and will attempt to automatically open that page.
#' @return The returned status of request
#' @export
#'
#'
upload_request <- function(csv_path, xml_path,
                                 login = NULL, password = NULL,
                                 link_path = "annotation_status_link.url"){

  CSV_upload <- fileUpload(filename = csv_path, contentType="text/plain")
  XML_upload <- fileUpload(filename = xml_path, contentType="text/xml")
  h <- basicHeaderGatherer()
  b <- basicHeaderGatherer()

  if(is.null(login)){
    login <- readline(prompt="Enter login: ")
    password <- readline(prompt="Enter password: ")
  }

  postForm(glue('{base_url}/request-annotation-xml-2.jsp'),
           request = XML_upload, tracks = CSV_upload,
           .params = list(login = login, password = password),
           .opts = curlOptions(headerfunction = h$update, writefunction = b$update, verbose = TRUE))

  url <- h$value()['Location']
  # cat("[InternetShortcut]\n URL=", "http://www.bioinfo.mpg.de/orn-gateway/",
  #     h$value()['Location'], file = link_path, sep = "")
  cat(glue("[InternetShortcut]\n URL={base_url}/{h$value()['Location']}"), file = link_path)
  # should use utils::browseURL, add prefix of http://www.bioinfo.mpg.de/orn-gateway/
  utils::browseURL(link_path)
  # try(shell(link_path))
  return(h$value())
}

##' Download and return annotated data from Env-DATA
##'
##' @param request_status the request_status returned by \code{\link{upload_request}}
##' @param file_path filename (and location) to save annotated data to.
##'
##' @return status of download
##' @export

download_result <- function(request_status, file_path = NULL){
  download_url <- glue("{base_url}/{request_status['Location']}") %>%
    str_replace("status.jsp", "download.jsp")
  download.file(download_url, file_path)
}
