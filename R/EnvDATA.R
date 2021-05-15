# functions relate to Movebank Env-DATA service
# internal request functions ----
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
  fill_if_not_available("name", format(lubridate::now(), "%Y-%m-%d_%H-%M-%S"))
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
    .opts = RCurl::curlOptions(headerfunction = h$update, writefunction = b$update)
  )
  if (is.null(csv_path)) form_parameters$tracks <- NULL
  do.call(RCurl::postForm, form_parameters)
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
# exported functions ----
# functions just generate parameter list. submit functions take para list and create xml, submit xml. user never need to deal with xml, only deal with parameter list.
# we can assume most track csv will not be ready for use, so we need to start from data.frame anyway. we can put convertion in pipeline so we don't do automatic conversion

##' Convert data frame to Movebank Env-Data track request format
##'
##' Takes a data frame of latitudes, longitudes and times and generates the
##' strictly formatted data frame needed to upload to Env-Data to obtain
##' covariates for movebank.
##'
##' @param df data.frame in [movebank format](https://www.movebank.org/node/13):
##'   longitude and latitude in WGS84, timestamp in POSIXct or ymd_hms format,
##'   and height above ellipsoid (in m). If no timezone information exist, it
##'   will be interpreted as UTC time. If no height above ellipsoid provided it
##' @param time_name column name for timestamp. The parameter default values
##'   matches ctmm telemetry object data.frame, so there is no need to specify
##'   them with telemtry data.frame.
##' @param long_name column name for longitude
##' @param lat_name column name for latitude
##' @param height_name column name for height above ellipsoid. This column is
##'   optional and the name can be left as NA
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

#' @describeIn submit_grid_request Submit Track Annotation Request
#' @param track_df A data.frame that in EnvData track request format, for
#'   example converted from `to_track_df`
#'
#' @export
submit_track_request <- function(track_df, para_list, user, password) {
  temp_csv_path <- tempfile(fileext = ".csv")
  fwrite(track_df, temp_csv_path)
  temp_xml_path <- tempfile(fileext = ".xml")
  save_request_xml("track", para_list, temp_xml_path)
  upload_request("track", temp_csv_path, temp_xml_path, user, password)
}
#' Submit EnvData Annotation Request
#'
#' @describeIn submit_grid_request Submit Grid Annotation Request
#' @param para_list Parameter list
#' @param user Movebank EnvData backend login (not the same as a MoveBank
#'   account), which you have to contact the Movebank to obtain
#' @param password Movebank EnvData backend password
#'
#' @return The status url, which can be feed to `download_result`
#' @export
submit_grid_request <- function(para_list, user, password) {
  temp_xml_path <- tempfile(fileext = ".xml")
  save_request_xml("grid", para_list, temp_xml_path)
  upload_request("grid", csv_path = NULL, temp_xml_path, user, password)
}
# it should be good if we also download the request xml and save together. also better save the status url.
# if download success, return csv path for track mode, extracted tiff folder for grid mode

#' Download Annotation Request Result
#'
#' Download request result after it's available. It may take some time for the
#' request to be finished.
#' - For track result, an annotated csv and the request xml will be saved.
#' - For grid result, the result zip will be downloaded and uncompressed,
#' and the request xml also saved.
#'
#' @param request_status_url The Request status url. `submit_track_request` and
#'   `submit_grid_request` will print the url and also return the url. The url
#'   can be used to check request status.
#' @param save_folder Where to save the result.
#'
#' @export
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
    if (stringr::str_detect(download_url, "zip")) {
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

# wrapper for ndvi/evi ----
# generate parameter list, pipe to submit request.

#' Create Grid Annotation Request from extent Result
#'
#' This is a wrapper function that you can create Annotation request with
#' minimal input.  The wrapper function can request NDVI or EVI values based on
#' the `ctmm::extent` result. `ctmm::extent` take a telemetry object or UD
#' object and return their spatial bounding box and time range.
#'
#' @param data_type The environment data to be requested. Currrently only NDVI
#'   and EVI are supported.
#' @param ext Return value from `ctmm::extent` applied on telemetry object which
#'   includes bounding box and time range. If applied to UD only bounding box is
#'   available, so it's suggested to feed `ctmm::extent` with a list of
#'   telemetry object and UD object so it will return time range from telemetry
#'   object and bounding box from the UD object.
#' @param buffer Increase the bounding box to make it cover the edge area. 0.1
#'   will extend 10% in each dimension.
#'
#' @return request parameter list, which can be feed to `submit_grid_request`.
#' @export

create_envdata_request <- function(data_type = c("NDVI", "EVI"), ext,
                                   buffer = 0) {
  # select product ----
  TILE_RESOLUTION_LIMIT <- 2000
  para_list_grid <- list()
  # first select a product, variable. could be multiple variables. usually we can have multiple variables under same product in one request. So first select one product, then select variables.
  # we seemed need to use different color map for different variable. so each request need to based on single variable. multi request doesn't have more burden for system unless we need to upload multiple times. it's possible we want to get multiple variables if they are similar enough, then we can merge them with a policy (if most are same and only different in variable name)
  # and save variable name could have multiple product name. determine these two explicitly.
  # be explicit. we can support two variables at same time if they are same resolution, but it's hard to control. depend on the UI in future. for now, just do individual variable which is most flexible.
  para_list_grid$variable_names <- switch(data_type,
                                          NDVI = "250m 16 days NDVI",
                                          EVI = "250m 16 days EVI")
  # para_list_grid$variable_names <- "250m 16 days NDVI"
  # para_list_grid$variable_names <- "250m 16 days EVI"
  # no real reason to choose Terra vs Aqua, unless user know the difference. could specify in parameter if needed. right now the config only have one option with these selections.
  selected_variable_row<- VARIABLE_DT[satellite == "Terra" &
                                        variable == para_list_grid$variable_names]
  # should ensure single selection
  stopifnot(nrow(selected_variable_row) == 1)
  para_list_grid$type_name <- selected_variable_row$product
  # same production, different variable of same resolution
  product_resolution <- selected_variable_row$resolution
  # product_start_time <- selected_variable_row$start_time
  # area range ----
  # let's use same format of bbox, convert extent to bbox format
  bb_tele <- list(xmin = ext["min", "longitude"], ymin = ext["min", "latitude"],
                  xmax = ext["max", "longitude"], ymax = ext["max", "latitude"])
  # TODO add buffer, we will need to change each point in different direction. usually that means keep center and increase on each direction. we cannot assume xmin < xmax, min max is only meant west/east, it's possible to have 179 ~ -179, right? (-179 ~ 179 mean a different range). this only happens in longitude since -180 jump to 180. check if international date line is in range, just check if 180/-180 in range, then add 360 to them, caculate, minus 360 (not mod, this is signal change, shift to positive then shift back)
  # check tmap::bb as example. didn't handle this properly
  # b1 <- tmaptools::bb(rbind(c(178, -178), c(60, 61)))
  # tmaptools::bb(b1, ext = 1.1)
  # refer to [ctmm median function](https://github.com/ctmm-initiative/ctmm/blob/master/R/circle.R#L7). will need to add special treatment when this became a package and checking for CRAN.
  # get extended bbox in linear range like -180 - 0 - 180. original bbox didn't cover -180/180 line
  normal_buffered <- function(bb, buffer) {
    extend_1D <- function(a, b, buffer) {
      center <- mean(c(a, b))
      half_range <- (b - a) / 2
      return(list(a_new = center - half_range * (1 + buffer),
                  b_new = center + half_range * (1 + buffer)))
    }
    bb[c("xmin", "xmax")] <- extend_1D(bb$xmin, bb$xmax, buffer)
    bb[c("ymin", "ymax")] <- extend_1D(bb$ymin, bb$ymax, buffer)
    return(bb)
  }
  # didn't cover the edge case of one end right on anti-meridian, less possible compare to one end on 0. assume alawys xmin < xmax in value
  # movebank cannot deal with this kind of data, have to split into two area, ask user, or do it by ourselves
  # if (bb_tele$xmin > -180 && bb_tele$xmin <= 0 && bb_tele$xmax >= 0 && bb_tele$xmax < 180) {
  #   # right side, min value
  #   bb_tele$xmin <- bb_tele$xmin + 360
  #   bb <- normal_buffered(bb_tele, buffer)
  #   bb$xmin <- bb$xmin - 360
  # } else {
  #   bb <- normal_buffered(bb_tele, buffer)
  # }
  # ctmm extent always make xmin the west side. so xmin > xmax means anti-meridian.
  if (bb_tele$xmin > bb_tele$xmax) stop("The grid area crossed anti-meridian. Movebank don't support this type of case, please split the grid into two parts and avoid anti-meridian.")
  bb_new <- normal_buffered(bb_tele, buffer)
  # need to convert bbox to origin + range
  # - origin (x,y): Enter the coordinates of the northwest corner of the area of interest (bounding box) in decimal degrees(long,lat).
  # if we assume xmin < xmax, xmin may not be west. though we assume ENV-Data just calculate bounding box in normal case, then the shape calculation can be problematic too.
  # long: jingdu, x; lat: weidu, height, always lon/lat which is x/y
  # bounding box 4 corners
  # xmin, ymax          xmax, ymax
  #
  # xmin, ymin          xmax, ymin
  # NW corner
  para_list_grid$origin <- c(bb_new$xmin, bb_new$ymax)
  # - shape (x,y): Enter the distance that the bounding box extends from the northwest corner in decimal degrees (long,lat; x should be positive and y should be negative)
  para_list_grid$shape <- c(bb_new$xmax - bb_new$xmin, bb_new$ymin - bb_new$ymax)
  # resolution. DEM resolution is arc-second, calculated with degree. With NDVI resolution at 250m, we just use range divided by 250m to get pixels. then split to tile smaller than 2000x2000, calculate per tile resolution.
  # if resolution is defined by arc-second, it's easy to calculate with just degree difference.
  # with 250m resolution, we need distances on ellipsoid. Note this use WGS84 by default. we also only calculated one edge in 4 edges, when top edge could be different from bottom edge
  x_range <- geosphere::distGeo(para_list_grid$origin, c(bb_new$xmax, bb_new$ymax))
  y_range <- geosphere::distGeo(para_list_grid$origin, c(bb_new$xmin, bb_new$ymin))
  # let's see what will happen with higher than product resolution
  resolution <- round(c(x_range, y_range)/product_resolution)
  # Please never ask for more than 1,000,000 points in a request. the bigger request was never requested, kept as new status
  if (resolution[1] * resolution[2] > 1e6) stop("Requested area too big. One request should not exceed 1,000,000 points.", "Current request is ", resolution[1] * resolution[2])
  # if it's > 2000, divide to tiles smaller than 2000. divide by 2000 then ceiling, that's the tile needed.
  para_list_grid$number_tiles <- ceiling(resolution/TILE_RESOLUTION_LIMIT)
  # get per tile resolution, need to be integer
  para_list_grid$number_pixels <- ceiling(resolution/para_list_grid$number_tiles)
  # let's try bigger resolution and see what's difference
  # para_list_grid$number_pixels <- c(2000, 2000)
  # time range ----
  # get product date, use center coordinates
  # not totally sure if the different version number have any difference. supposedly the raw data is same.
  # MODISTools::mt_products()
  product_name <- para_list_grid$type_name %>% stringr::str_remove(".*/") %>% stringr::str_remove("\\..*")
  product_dates <- MODISTools::mt_dates(product_name,
                                        lat = mean(bb_new$ymin, bb_new$ymax),
                                        lon = mean(bb_new$xmin, bb_new$xmax)) %>% data.table
  # subset the dates within time period, we need the time range vector (min, max)
  # note we didn't specify hours, which doesn't matter too much.
  covered_dates <- product_dates[calendar_date >= format(ext$timestamp[1], "%Y-%m-%d") &
                                   calendar_date <= format(ext$timestamp[2], "%Y-%m-%d"), calendar_date]
  para_list_grid$timestamps <- covered_dates %>% (lubridate::ymd) %>% format("%Y-%m-%d %H:%M:%S")
  # should use product color scheme. then we cannot request ndvi and evi in same request?
  if (!is.na(selected_variable_row$color_map)) para_list_grid$color_map <- selected_variable_row$color_map
  return(para_list_grid)
}

# let's make it simple, each request only return one product. so only one folder after decompress. we can continue after the decompress
# assume single subfolder. also assume tiff. we can feed download function result to this function.
# mosaic can comput overlap cells. merge use one layer to override

#' Convert geotiff Result into Raster Stack
#'
#' Grid request result is in geotiff format by default. The grid could be split
#' in tiles if too big, and each timestamp will have a snapshot. This function
#' will merge split tiles if existed, convert geotiff into `raster::raster`,
#' merge multiple snapshots into a `raster::RasterStack`, put snapshot name into
#' `Z` slot.
#'
#' @param tiff_folder The path to tiff files
#'
#' @return A `raster::RasterStack`
#' @export
process_data_folder <- function(tiff_folder) {
  files <- list.files(tiff_folder, pattern = ".tif", full.names = TRUE)
  file_name_pieces <- basename(files) %>% stringr::str_match(".+(-\\d+)-(\\d)-(\\d)\\.tif") %>% data.table
  # easier to test a matrix which is just an array.
  if ("1" %in% (file_name_pieces[, 3:4] %>% as.matrix)) {
    # put merged or renamed files into a subfolder
    final_folder <- file.path(tiff_folder, "merged")
    if (file.exists(final_folder)) unlink(final_folder, recursive = TRUE)
    dir.create(final_folder)
    # warning("Split tiles will be merged into single file, please keep the downloaded zip as backup.")
    merge_tiles <- function(tiles) {
      # tiles <- file_name_pieces[1:3, V1]
      raster_list <- purrr::map(file.path(tiff_folder, tiles), raster::raster)
      raster_list$filename <- file.path(final_folder, stringr::str_remove(tiles, "-\\d-\\d") %>% unique)
      do.call(raster::merge, raster_list)
      # log_msg("info", "merging tiles into", raster_list$filename)
      return(raster_list$filename)
    }
    log_msg("info", "Merging tiles into single file")
    file_name_pieces[, merge_tiles(V1), by = V2]
  } else {
    log_msg("info", "Removing tile index since they are single tiles")
    file.rename(files, stringr::str_remove(files, "-\\d-\\d"))
    final_folder <- tiff_folder
  }
  # build stack, get time from name, put into Z --
  res_stack <- list.files(final_folder, full.names = TRUE) %>% stack
  # list.files(tiff_folder, full.names = TRUE) %>% brick
  # get time, assume the naming pattern. or just use our original time vector? the former rely on backend code, the latter may not always true. though it should be true, just check length should be enough. however to get the time series need to save it from somewhere, or write a function to get it from data, then it depend on product selection.
  # for now just take from names
  times <- names(res_stack) %>% stringr::str_extract("\\.\\d+") %>%
    stringr::str_sub(2, 15) %>% lubridate::ymd_hms
  res_stack <- raster::setZ(res_stack, times)
  # it took quite some time to create brick, and it didn't save time?
  # brick(res_stack)
  return(res_stack)
}
