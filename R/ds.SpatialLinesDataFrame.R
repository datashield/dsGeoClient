#' 
#' @title Wrapper for \code{SpatialLinesDataFrame()} function from sp package
#' @description This function is a wrapper for the \code{SpatialLinesDataFrame()} function from the
#' sp package
#' @details See the \code{SpatialLinesDataFrame()} function from sp package for more details
#' @param x name of an object on the server side of class "SpatialPointsDataFrame" 
#' or "SpatialPoints" for which the epsg coordinate system will be transformed
#' @param projStr a valid proj4 epsg coordinate system identifier e.g. 29902 for
#' Ireland. (N.B. need to add validation to check the id)
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.trans'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return either an object of class SpatialPointsDataFrame or SpatialPoints, 
#' depending on the class of x
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
ds.SpatialLinesDataFrame = function(lines=NULL, data=NULL, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(lines)){
    stop("Please provide the name of a spatialLines object!", call.=FALSE)
  }
  
  # check if a valid data frame has been provided
  if(is.null(data)){
    stop("Please provide a valid data frame!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined_lines <- isDefined(datasources, lines)
  defined_data <- isDefined(datasources, data)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, lines)
  
  # if the input object is not a matrix or a dataframe stop
  if(typ != 'SpatialLines'){
    stop("The input 'lines' must be of type 'SpatialLines'!", call.=FALSE)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ2 <- checkClass(datasources, data)
  
  # if the input object is not a matrix or a dataframe stop
  if(typ2 != 'data.frame'){
    stop("The input 'data' must be of type 'data.frame'!", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- paste0(lines,".df")
  }
  
  # call the server side function and do the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Transforming coordinate system on ", names(datasources)[i], "..."))
    cally <- paste0("SpatialLinesDataFrameDS(", lines,",",data,")")
    print(cally)
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
}
