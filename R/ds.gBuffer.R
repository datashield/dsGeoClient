#' 
#' @title Wrapper for \code{gBuffer()} function from sp and rgeos packages
#' @description This function is a wrapper for the \code{gBuffer()} function from the
#' sp and rgeos packages
#' @details See the \code{gBuffer()} function from rgeos package for more details
#' @param input name of an object on the server side of class "SpatialLinesDataFrame" 
#' or "SpatialLines" to which the buffer will be applied
#' @param by_id Logical determining if the function should be applied across subgeometries 
#' (TRUE) or the entire object (FALSE)
#' @param ip_width Distance from original geometry to include in the new geometry. 
#' Negative values are allowed. Either a numeric vector of length 1 when byid is 
#' FALSE; if byid is TRUE: of length 1 replicated to the number of input geometries,
#'  or of length equal to the number of input geometries
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.buff'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return an object of class SpatialPolygonsDataFrame or SpatialPolygons, 
#' depending on the class of input
#' @author Bishop, T.
#' @export
#' @examples {
#' 
#' # Transform epsg coordinate system for a SpatialPointsDataFrame called mySPframe
#' # The coordinate system 29902 is the code for Ireland
#' 
#' ds.spTransform('mySPframe',29902)
#' 
#' }
#' 
ds.gBuffer = function(input=NULL, by_id=FALSE, ip_width=NULL, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(input)){
    stop("Please provide the name of a spatial points data frame!", call.=FALSE)
  }
  
  # check if a valid width has been provided
  if(is.null(ip_width)){
    stop("Please provide a valid width for the buffer!", call.=FALSE)
  }
  
  if(!is.numeric(ip_width)){
    stop("Buffer width is not numeric!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, input)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, input)
  print(typ)
  # if the input object is not a matrix or a dataframe stop
  if(typ != 'SpatialLinesDataFrame' & typ != 'SpatialLines'){
    stop("The input vector must be of type 'SpatialLinesDataFrame' or 'SpatialLines!", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- paste0(input,".buff")
  }
  
  # call the server side function and do the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Creating buffer on geometry for ", names(datasources)[i], "..."))
    cally <- paste0("gBufferDS(", input,",",by_id,",",ip_width,")")
    print(cally)
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
}
