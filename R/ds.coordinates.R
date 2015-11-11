#' 
#' @title Wrapper for \code{coordinates()} function from sp package
#' @description This function is a wrapper for the coordinates() function from the
#' sp package
#' @details See the \code{coordinates()} function from sp package for more details
#' @param x name of an object on the server side of class "data.frame" to be 
#' converted to a spatial object
#' @param coords spatial coordinates; column names (e.g. c("x","y")) specifying 
#' which columns in object are the spatial coordinates.
#' If the coordinates are part of object, giving the reference does not
#' duplicate them, giving their value does duplicate them in the resulting structure. 
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.coords'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return usually an object of class SpatialPointsDataFrame; if the coordinates
#' set cover the full set of variables in object, an object of class SpatialPoints
#' is returned
#' @author Bishop, T.
#' @export
#' @examples {
#' 
#' # Convert data frame D to a SpatialPointsDataFrame
#' # Data frame D has columns Lon and Lat which contain the coordinates
#' 
#' myvect <- c("Lon","Lat")
#' ds.coordinates('D',myvect)
#' 
#' }
#' 
ds.coordinates = function(x=NULL, coords=NULL, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of a data frame!", call.=FALSE)
  }
  
  # check if coordinate names have been provided
  if(is.null(coords)){
    stop("Please provide a list of coordinate column names!", call.=FALSE)
  }
  
  if(!is.character(coords)){
    stop("List of coordinate column names is not a character!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # if the input object is not a matrix or a dataframe stop
  if(typ != 'data.frame'){
    stop("The input vector must be of type 'data.frame'!", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- paste0(x,".coords")
  }
  
  #check whether the coordinates columns exist on the server side
  for(i in 1:length(datasources)){
    col_check <- unlist(ds.colnames(x,datasources[i]))
    if(!(all(is.element(coords,col_check)))){
      
      stop("The column names specifying the coordinates are not found in
           the target data frame!", call.=FALSE)
      
    }
  }
  
  #need a check for whether the columns are numeric
  
  #need a check that there are more than 1 coordinate columns specified.
  
  # call the server side function and do the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Converting ", names(datasources)[i], "data frame to coordinates object"))
    cally <- paste0("coordinatesDS(", x,", c(","'",paste(coords,collapse="','"),"')",")")
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
    }
  
}
