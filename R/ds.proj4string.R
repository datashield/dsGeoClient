#' 
#' @title Wrapper for \code{proj4string()} function from spand rgdal packages
#' @description This function is a wrapper for the proj4string() function from the
#' sp and rgdal packages
#' @details See the \code{proj4string()} function from sp package for more details
#' @param x name of an object on the server side of spatial class to which the epsg coordinate system will be assigned
#' @param projStr a valid proj4 epsg coordinate system identifier e.g. 29902 for
#' Ireland. 
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.proj'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return either an object of class SpatialPointsDataFrame or SpatialPoints, 
#' depending on the class of x
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
#' }
#' 
ds.proj4string = function(x=NULL, projStr=NULL, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of a spatial points object!", call.=FALSE)
  }
  
  # check if a valid proj4 epsg coordinate system identifier string has been provided
  if(is.null(projStr)){
    stop("Please provide a a valid proj4 epsg coordinate system identifier!", call.=FALSE)
  }
  
  if(!is.numeric(projStr)){
    stop("Proj4 epsg coordinate system identifier is not a number!", call.=FALSE)
  }
  
  #could do away with this and split genuine CRS objects, and reconstruct on the other
  # side - can't pass the + and = over DataSHIELD
  # however it might require error checking on the client side that it is a valid
  # CRS object....
  
  #projStrSplit <- gsub(" ","",unlist(strsplit(projStr, split="\\+")))
  #projStrSplit <- do.call(rbind,strsplit(projStrSplit[-1], '\\='))
  
  #to rebuild on the server side
  #paste(paste("+",paste(projStrSplit[,1],projStrSplit[,2],sep="="),sep=""),collapse=" ")
  
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  # if the input object is not a matrix or a dataframe stop
#   if(typ != 'SpatialPointsDataFrame' & typ != 'SpatialPoints'){
#     stop("The input vector must be of type 'SpatialPointsDataFrame'!", call.=FALSE)
#   }
  
  if(is.null(newobj)){
    newobj <- paste0(x,".proj")
  }
  
  # call the server side function and do the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Assigning coordinate system to points object on ", names(datasources)[i], "..."))
    cally <- paste0("proj4stringDS(", x,",",projStr,")")
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
}
