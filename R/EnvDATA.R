# functions relate to Movebank Env-DATA service
# prep track data ----

##' Convert data frame to Movebank Env-Data track request file
##'
##' Takes a data frame of latitudes, longitudes and times and generates the
##' strictly formatted data frame needed to upload to Env-Data to obtain
##' covariates for movebank.
##'
##' @param df data.frame in [movebank format](https://www.movebank.org/node/13):
##'   longitude and latitude in WGS84, timestamp in POSIXct or ymd_hms format,
##'   and height above ellipsoid (in m). If no timezone information exist, it
##'   will be interpreted as UTC time. If no height above ellipsoid provided it
##' @param time_name column name for timestamp
##' @param long_name column name for longitude
##' @param lat_name column name for latitude
##' @param height_name column name for height above ellipsoid. This column is optional and the name can be left as NA
##' @examples
##' library(ctmm)
##' library(data.table)
##' data(buffalo)
##' df <- data.frame(buffalo[[1]])
##' dt <- to_track_df(df)
##'
##' @export
to_track_df <- function(df, time_name = "timestamp",
                        long_name = "longitude",
                        lat_name = "latitude",
                        height_name = NA){
  dt <- data.table(df)
  if (nrow(dt) > 1e6) stop("Cannot make request with more than 1,000,000 points")
  if (is.na(height_name)) {
    dt[, `height-above-ellipsoid` := ""]
    height_name <- "height-above-ellipsoid"
  }
  setnames(dt, c(time_name, long_name, lat_name, height_name),
           c("timestamp", "location-long", "location-lat", "height-above-ellipsoid"))
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

# request functions ----

complete_parameters <- function(request_type, para_list) {
  # conversion function none should write as "", not NA/NULL, or just skip. In grid mode it can be "", but in track mode it must be skipped if none.
  # special treatment not recorded in config dt: rescale_color flag, add fixed-double prefix in colormap. outputformat have different value in option(same with web page) and internal values
  # 1. take config by request type
  config_dt <- CONFIG_ALL_DT[type == request_type]
  # 1.1 adjust value
  # we are making list into data.frame, cannot really take a vector as value. use string value directly.
  # timestamps also need conversion. all these are for grid type
  # browser()
  # convert the vector values into a comma separated string
  all_vec_to_string <- function(vec_cols, collapse_symbol) {
    purrr::walk(vec_cols, ~ {
      # this doesn't change outside as inside walk it's in a function.
      para_list[[.]] <<- stringr::str_c(para_list[[.]], collapse = collapse_symbol)
    })
  }
  # simple parse back to vec in case needed.
  string_to_vec <- function(vec_str, sep_symbol) {
    stringr::str_split(vec_str, sep_symbol)[[1]] %>% as.numeric
  }
  # need to use parameter name here, update with config changes. try to use config so we don't need to update code when updating config
  # all_vec_to_string(intersect(c("variable_names", "variable_labels"), names(para_list)), ",")
  all_vec_to_string(intersect(config_dt[add_separator == "comma", parameter], names(para_list)), ",")
  all_vec_to_string(intersect(config_dt[add_separator == "colon", parameter], names(para_list)), ":")
  if (request_type == "grid") {
    # first conver timestamps. seemed to be seconds * 1000 to have 0.001 sec resolution(some platform may have time at 0.001 sec resolution). with bigger number numeric calculation will change to scientific notation. use string operation instead
    # para_list$timestamps <- ((lubridate::ymd_hms(para_list$timestamps) %>% as.numeric) * 1000) %>% stringr::str_c(collapse = ":")
    para_list$timestamps <- ((lubridate::ymd_hms(para_list$timestamps) %>% as.numeric %>% as.character) %>%
                               stringr::str_c("000")) %>% stringr::str_c(collapse = ":")
  }
  input_dt <- para_list %>% as.data.frame %>% t %>% data.table(keep.rownames = TRUE) %>%
    setnames(1:2, c("parameter", "input"))
  # 2. check parameters
  # 2.1 make sure all parameter name matches (is a subset)
  para_diff <- setdiff(input_dt$parameter, config_dt$parameter)
  if (length(para_diff) > 0) stop(glue::glue("Parameter name wrong:\n{stringr::str_c(para_diff, collapse = '\n')}"))
  # 2.2 merge input parameter table and parameter template table
  config_dt <- merge(config_dt, input_dt, by = "parameter", all.x = TRUE, sort = FALSE)
  # we can drop parameter, type, require_input after check now to make later add new rows easier
  missed_input <- config_dt[(require_input) & is.na(input)]
  if (nrow(missed_input) > 0) stop(glue::glue("Some parameters are required but missed:\n{stringr::str_c(missed_input$parameter, collapse = '\n')}"))
  dt <- config_dt[, .(internal_name, options, level, input, value = input)]
  # 2.3 add/fill values
  # need to fill in some default values which are required by system but can omit from user input. these cannot be covered by "fill in first option rule". check all not required rows that don't have default value should be covered here.
  # 2.3.1 some should be added by program, not provided by user
  # 2.3.1.1 annotation type
  add_1 <- data.table(internal_name = "annotationType", level = "request",
                      value = stringr::str_c(request_type, "2"))
  # 2.3.1.2 type simple
  add_2 <- data.table(internal_name = "type", level = "property", value = "simple")
  # 2.3.1.3 for grid: temporalRasterType, typeName from type-name
  add_3 <- if (request_type == "grid") {
    add_g1 <- data.table(internal_name = "temporalRasterType", level = "request", value = "fixed-timestamps")
    add_g2 <- data.table(internal_name = "typeName", level = "element",
                         value = dt[internal_name == "type-name", input])
    rbindlist(list(add_g1, add_g2))
  }  else data.table()
  dt <- rbindlist(list(dt, add_1, add_2, add_3), fill = TRUE, use.names = TRUE)
  # 2.3.2 If not provided by user, we can fill in with default values. default value could be convention or first of option values.
  fill_if_not_available <- function(item_name, item_value) {
    if (is.na(dt[internal_name == item_name, value])) dt[internal_name == item_name, value := item_value]
  }
  # 2.3.2.1 generate name
  fill_if_not_available("name", format(now(), "%Y-%m-%d_%H-%M-%S"))
  # 2.3.2.2 fill variableLabel from variable-names
  fill_if_not_available("variableLabel", dt[internal_name == "variable-names", value])
  # 2.3.2.3 for grid: fill crsCode as EPSG:4326, fill numTiles as 1:1, gridsize as 2000:2000
  if (request_type == "grid") {
    fill_if_not_available("crsCode", "EPSG:4326")
    fill_if_not_available("numTiles", "1:1")
    fill_if_not_available("gridSize", "2000:2000")
    # make sure request points < 1e6. grid size is x, y size, tile count should be x, y count. should first calculate merged tile grid.
    # we need to parse value again because some value might not be part of parameters and filled with default
    tile_vec <- dt[internal_name == "numTiles", value] %>% string_to_vec(":")
    grid_vec <- dt[internal_name == "gridSize", value] %>% string_to_vec(":")
    total_dimension <- tile_vec * grid_vec
    if (total_dimension[1] * total_dimension[2] > 1e6) stop("Requested area too big. One request should not exceed 1,000,000 points.", "Current request is ", total_dimension[1] * total_dimension[2])
  }
  # 2.3.3 if an input is empty and have options value, fill in first option to value. note options are character column, empty value is "" instead of NA
  # we don't want to verctorize inside data.table, as different row can have different result count.
  if (nrow(dt[options != "" & is.na(value)] > 0)) {
    first_values <-  (dt[options != "" & is.na(value), options] %>%
                        stringr::str_split("\\|", simplify = TRUE))[, 1]
    dt[options != "" & is.na(value), value := first_values]
  }
  # the value is string "TRUE"/"FALSE". we need to change color map if it's true, but need to remove the row no matter what in grid mode
  if (request_type == "grid") {
    # check output format (now it has value after fill in default)
    dt[internal_name == "outputFormat" & value == "kmz", value := "png/kmz"]
    dt[internal_name == "outputFormat" & value == "geotiff", value := "tif"]
    dt[internal_name == "colorMapSpec" & value != "trivial", value := stringr::str_c("fixed-double:", value)]
    # rescale and remove rescale flag
    if (dt[internal_name == "rescale_color", value == "TRUE"]) {
      dt[internal_name == "colorMapSpec", value := stringr::str_c(value, ":rescale")]
    }
    dt <- dt[internal_name != "rescale_color"]
  }
  # 2.4 make sure now all rows have value, either from user input, or default, or manuall filled. this should pass with minimal parameter value call.
  # now we also skip email, so that row chould be NA. let's just skip all NA/empty rows, give warnings and don't stop here unless had problem.
  # if (nrow(dt[is.na(value)]) > 0 ) {
  #   # cat(dt[is.na(value), internal_name])
  #   # browser()
  #   warning("Some parameter values not avaiable: \n", stringr::str_c(dt[is.na(value), internal_name], collapse = "\n"))
  #   # cat(dt[is.na(value), internal_name], sep = "\n")
  #   }
  # track mode cannot take conversion function as empty, need to skip it. we cannot edit the config as it will conflict with above logic. need to remove it here. though in general anything with "" value can be skipped. for now only conversion function is like this.
  return(dt[(value != "") & (!is.na(value))])
}
build_save_xml <- function(para_dt, xml_path) {
  # the named list version cannot handle name with -, which was back ticked
  property_dt <- para_dt[level == "property"]
  property_node <- XML::xmlNode("Properties",
                                .children = purrr::map(1:nrow(property_dt), ~ {
                                  XML::xmlNode("AnnotationRequestElementProperty",
                                               attrs = list(name = property_dt[., internal_name],
                                                            value = property_dt[., value]))
                                }))
  # other two level can use named list directly
  dt_to_named_list <- function(level_name) {
    dt_temp <- para_dt[level == level_name, .(internal_name, value)]
    named_vec <- as.list(dt_temp$value)
    names(named_vec) <- dt_temp$internal_name
    return(as.list(named_vec))
  }
  element_node <- XML::xmlNode("AnnotationRequestElement", attrs = dt_to_named_list("element"),
                               property_node)
  request_xml <- XML::xmlNode("AnnotationRequest", attrs = dt_to_named_list("request"),
                              XML::xmlNode("Elements", element_node))
  # in normal request this was saved to temp file. debug mode show the temp file path
  log_msg("debug", "Request XML saved to", xml_path)
  XML::saveXML(request_xml, file = xml_path, encoding = "UTF-8")
}
# need this intermediate function to save xml and compare with template
save_request_xml <- function(request_type, para_list, xml_path) {
  para_dt <- complete_parameters(request_type, para_list)
  # when we build a named list, the name with - in propery node will have `` quote around it, then build xml part cannot find the name properly. previously we write with "" name explicitly which don't have problem.
  build_save_xml(para_dt, xml_path)
}
# grid mode don't have csv. in track mode we can also use previously upload csv and skip it.
# why call user as login? change it, but need to use login internally as this is what it called in movebank document.
upload_request <- function(request_type, csv_path, xml_path,
                           user, password){
  # CSV_upload <- switch(request_type,
  #                      track = fileUpload(filename = csv_path, contentType="text/plain"),
  #                      grid = NULL)
  CSV_upload <- if (is.null(csv_path)) NULL else RCurl::fileUpload(filename = csv_path, contentType="text/plain")
  XML_upload <- RCurl::fileUpload(filename = xml_path, contentType="text/xml")
  # looks like h to gather response, b to upload file? not sure.
  h <- RCurl::basicHeaderGatherer()
  b <- RCurl::basicHeaderGatherer()
  if(is.null(user)){
    user <- readline(prompt = "Enter user: ")
    password <- readline(prompt = "Enter password: ")
  }
  # two page for two request separately. cannot switch
  request_url <- switch(request_type,
                        track = glue::glue('{BASE_URL}/request-annotation-xml-2.jsp'),
                        grid =  glue::glue('{BASE_URL}/request-annotation-xml.jsp'))
  form_parameters <- list(
    uri = request_url, request = XML_upload, tracks = CSV_upload,
    .params = list(login = user, password = password),
    .opts = RCurl::curlOptions(headerfunction = h$update, writefunction = b$update, verbose = debug_mode)
  )
  if (is.null(csv_path)) form_parameters$tracks <- NULL
  do.call(postForm, form_parameters)
  # browser()
  # possible error: sometimes there is jsp error. sometimes looks normal but no access key info.
  # with track request, success status is 302. just check the key url part is better.
  # report error and stop early if response is not successful? but with failed request the status is still 200. maybe only show some in debug mode?
  # if (h$value()["status"] != "200") stop(glue::glue("Request failed: {h$value()['statusMessage']}"))
  # track response have it in Location part, grid response have it in access-key part
  key_url <- switch(request_type,
                    track = h$value()['Location'],
                    grid = glue::glue("status.jsp?access-key={h$value()['access-key']}"))
  # at least don't open a wrong web page and give proper message
  if (is.na(key_url)) stop("Request failed, check request xml again")
  status_url <- glue::glue("{BASE_URL}/{key_url}")
  # return the value, also print on console in case didn't save it
  cat("Check request status on: ", status_url, sep = "\n")
  # some platforms accept file path, so not always work with file? save a url path is also not really great.
  utils::browseURL(status_url)
  # try(shell(link_path))
  return(invisible(status_url))
}
# we can assume most track csv will not be ready for use, so we need to start from data.frame anyway. we can put convertion in pipeline so we don't do automatic conversion
submit_track_request <- function(track_df, para_list, user, password) {
  temp_csv_path <- tempfile(fileext = ".csv")
  fwrite(track_df, temp_csv_path)
  temp_xml_path <- tempfile(fileext = ".xml")
  save_request_xml("track", para_list, temp_xml_path)
  upload_request("track", temp_csv_path, temp_xml_path, user, password)
}
submit_grid_request <- function(para_list, user, password) {
  temp_xml_path <- tempfile(fileext = ".xml")
  save_request_xml("grid", para_list, temp_xml_path)
  upload_request("grid", csv_path = NULL, temp_xml_path, user, password)
}
# it should be good if we also download the request xml and save together. also better save the status url.
# if download success, return csv path for track mode, extracted tiff folder for grid mode
download_result <- function(request_status_url, save_folder = "."){
  # check status ----
  # need to know it's track or grid result, detect keyword in result page. this may change, give warning if failed to detect.
  # also need to check if result is available.
  status_page_content_file <- tempfile()
  download.file(request_status_url, status_page_content_file, quiet = TRUE)
  status_page_content <- readLines(status_page_content_file)
  request_status <- status_page_content %>% stringr::str_subset("Status:") %>% stringr::str_replace("&nbsp;", "")
  if (!stringr::str_detect(request_status, "available|delivered")) stop(glue::glue("Result not available yet:\n\t {request_status}"))
  # prepare download ----
  download_url <- stringr::str_replace(request_status_url, "status.jsp", "download.jsp")
  # take first 6 digits of access key to name result file
  result_name <- stringr::str_extract(request_status_url, "=.*") %>% stringr::str_sub(2, 7)
  # start download ----
  # save the request url for future reference
  url_target_path <- file.path(save_folder, glue::glue("{result_name}_url.txt"))
  cat(request_status_url, file = url_target_path)
  log_msg("info", "Request Status url saved", url_target_path)
  # download request xml as some metadata
  # http://www.bioinfo.mpg.de/orn-gateway/request-info-xml.jsp?access-key=
  request_xml_url <- stringr::str_replace(request_status_url, "status.jsp", "request-info-xml.jsp")
  target_xml_path <- file.path(save_folder, glue::glue("{result_name}_request.xml"))
  log_msg("info", "Downloading Request XML", target_xml_path)
  download.file(request_xml_url, target_xml_path)
  if (any(stringr::str_detect(status_page_content, "input file"))) {
    # track mode
    target_file_path <- file.path(save_folder, glue::glue("{result_name}.csv"))
    log_msg("info", "Downloading annotated track file...", target_file_path)
  } else {
    # grid mode
    download_url <- glue::glue("{download_url}&archive-format=zip")
    target_file_path <- file.path(save_folder, glue::glue("{result_name}.zip"))
    log_msg("info", "Downloading annotated grid files as single zip...", target_file_path)
  }
  # need to use mode wb for binary file. it can set wb if file extension is zip, but there is no file extension in url.
  download_status <- download.file(download_url, target_file_path, mode = "wb")
  if (download_status == 0) {
    # log_msg("info", "Download finished", target_file_path)
    if (str_detect(download_url, "zip")) {
      target_unzip_folder <- file.path(save_folder, result_name)
      # need to keep original folder structure as there could be multiple variables.
      uncompressed_files <- utils::unzip(target_file_path, exdir = target_unzip_folder)
      log_msg("info", glue::glue("Zip uncompressed"), uncompressed_files)
      # further pack into a raster stack. why list.dir always include the starting dir?
      tiff_folder <- setdiff(list.dirs(target_unzip_folder), target_unzip_folder)
      # TODO expect to be single sub folder
      return(tiff_folder)
    } else {
      return(target_file_path)
    }
  } else {
    log_msg("info", "Download failed.")
  }
}

# request envdata on ndvi/evi ----

