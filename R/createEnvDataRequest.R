# Based on E. Gurarie's [ABoVE](https://github.com/ABoVE-AotM/above), with modifications
#' Creates EnvData XML query
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
#' createEnvDataRequest("MyRequest", type_name, variable_name, interpolation_type, savefile = TRUE, fileout = "testfile.xml")

createEnvDataRequest  <- function(request_name = "MyRequest",
                                  type_name, variable_name,
                                  interpolation_type,
                                  dist_func_spatial="geodetic",
                                  email = "me@my.place",
                                  savefile = TRUE,
                                  fileout = NULL) {

  variable_label <- paste(type_name, "/", variable_name, sep = "")

  a <- xmlNode("Properties", xmlNode("AnnotationRequestElementProperty", attrs=list(name="interpolation-type", value=interpolation_type)),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="type-name", value=type_name)),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="type", value="simple")),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="distance-function-spatial", value=dist_func_spatial)),
               xmlNode("AnnotationRequestElementProperty", attrs=list(name="variable-names", value=variable_name)))
  b <- xmlNode("AnnotationRequestElement", attrs=c(variableLabel=variable_label), a)
  c <- xmlNode("Elements", b)

  Request_xml <- xmlNode("AnnotationRequest", attrs=list(annotationType="track2", name=request_name, notificationEmail=email), c)

  if(savefile){
    if(is.null(fileout))
      fileout <- readline(prompt="Please provide filename for XML request: ")
    cat("Saving xml request to", fileout, "\n")
    cat(saveXML(Request_xml), file = fileout)
  }
  else return(Request_xml)
}
