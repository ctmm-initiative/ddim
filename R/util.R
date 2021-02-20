# util functions
# format to env data required timestamp
format_time <- function(time_vec) {
  # time format: 2007-04-20 0:00:00.000 in UTC. ?format.POSIXct
  time_format <- "%Y-%m-%d %H:%M:%OS3"
  # format(Sys.time(), time_format)
  # with_tz(Sys.time(), tzone = "UTC")
  format(lubridate::with_tz(time_vec, tzone = "UTC"), time_format)
}
# print log message with color, controled by global option
LOG_colors <- list(warning = crayon::yellow,
                   info = crayon::green,
                   debug = crayon::red,
                   detail = crayon::blue)
# only message with level higher than option level get printed. detail will be in 2nd line with code format
log_msg <- function(level, msg, detail = "") {
  log_level_value <- c(none = 0, warning = 1, info = 2, debug = 3)
  current_level <- getOption("logLevel", default = "info")
  # for example, current level info = 2, warning and info can show but debug not shown.
  if (log_level_value[level] <= log_level_value[current_level]) {
    # some detail are vec
    if (length(detail) > 1 || detail != "") {
      detail <- stringr::str_c("\n\t", detail)
    }
    cat(LOG_colors[[level]](glue("[{level}]:")),
        LOG_colors[[level]](msg),
        LOG_colors$detail(detail), "\n")
  }
}
