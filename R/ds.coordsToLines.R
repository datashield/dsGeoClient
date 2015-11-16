#' 
#' @title Function to convert groups of points to lines
#' @description This function converts a SpatialPointsDataFrame object into a
#'  SpatialLines object
#' @details The input object consists of sets of points grouped by an identifier. The
#' function takes each group of points and converts them into a line with an associated
#' identifier
#' @param coords object of class SpatialPointsDataFrame to be converted
#' to a SpatialLinesDataFrame
#' @param group string indicating the name of the column in the data frame that defines
#' the grouping of the sets of points
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.lines'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return  an object of class SpatialLines, each line having an ID taken from the column
#' specified by the group variable
#' @author Bishop, T.
#' @export
#' @examples {
#' 
#' # Load log in data
#' 
#' data(GEOSPATIAL_logindata)
#' 
#' # login 
#' # (by default the assigned dataset is a dataframe named 'D')
#' opals <- datashield.login(logins=GEOSPATIAL_logindata,assign=TRUE)
#' 
#' # Convert data frame D to a SpatialPointsDataFrame
#' # Data frame D has columns Lon and Lat which contain the coordinates
#' 
#' myvect <- c("Lon","Lat")
#' ds.coordinates('D',myvect, newobj='coords')
#' 
#' # Assign epsg coordinate system to a SpatialPointsDataFrame called mySPframe
#' # The coordinate system 4326 is the code for WGS84 (GPS)
#' 
#' ds.proj4string('coords',4326,'mySPframe')
#' 
#' # Transform epsg coordinate system for a SpatialPointsDataFrame called mySPframe
#' # The coordinate system 29902 is the code for Ireland
#' 
#' ds.spTransform('mySPframe',29902,'transformed')
#' 
#' # Transform groups of points in a SpatialPointsDataFrame called transformed into lines
#' # The points in the data frame are grouped by an idenifier in a column called id
#' 
#' ds.coordsToLines('transformed','person_id',newobj='my_lines')
#' 
#' }
#' 
ds.coordsToLines = function(coords=NULL, group=NULL, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(coords)){
    stop("Please provide the name of a spatial points data frame!", call.=FALSE)
  }
  
  # check if a valid column name in the data frame has been provided as a string
  if(is.null(group)){
    stop("Please provide a a valid column name to group by!", call.=FALSE)
  }
  
  if(!is.character(group)){
    stop("Column name is not a string!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, coords)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, coords)
  print(typ)
  # if the input object is not a matrix or a dataframe stop
  if(typ != 'SpatialPointsDataFrame' & typ != 'SpatialPoints'){
    stop("The input vector must be of type 'SpatialPointsDataFrame'!", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- paste0(coords,".lines")
  }
  
  # call the server side function and do the work for each server
  for(i in 1:length(datasources)){
    message(paste0("--Converting points to lines on ", names(datasources)[i], "..."))
    cally <- paste0("coordsToLinesDS(", coords,",'",group,"')")
    print(cally)
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
}
