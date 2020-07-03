# Based on E. Gurarie's [ABoVE](https://github.com/ABoVE-AotM/above), with modifications
##' Download and return annotated data from EnvData
##'
##' @param header the header file outputted by \code{\link{uploadEnvDataRequest}}
##' @param returndata whether to return the data directly
##' @param filename filename (and location) to save annotated data to.  If left empty, user will be prompted.
##'
##' @return the annotated data frame
##' @export


downloadEnvData <- function(header, returndata = TRUE, filename = NULL){
  basicurl <- header['Location']
  downloadurl <- gsub("status.jsp", "download.jsp", basicurl)
  if(is.null(filename))
    filename <- readline(prompt="Please provide filename for annotated data: ")
  download.file(downloadurl, filename)
  return(read.csv(filename))
}
