#' 
#' @title Wrapper for \code{over()} function from sp and rgeos packages
#' @description This function is a wrapper for the \code{over()} function from the
#' sp and rgeos packages. Consistent spatial overlay for points, grids and polygons: 
#' at the spatial locations of object x retrieves the indexes or attributes from spatial 
#' object y
#' @details See the \code{over()} function from sp package for more details
#' @param x geometry (locations) of the queries (spatial object)
#' @param y layer from which the geometries or attributes are queried (spatial object)
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.over'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return either an object of class SpatialPointsDataFrame or SpatialPoints, 
#' depending on the class of x
#' @author Bishop, T.
#' @export
#' @examples {
#' 
#' 
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
#' # Set a buffer of 150 around points contained in the bus_work object
#' # Buffer is created per id in the bus_work object
#' 
#' ds.gBuffer('my_lines',by_id=,ip_width=150,'my_buffer')
#' 
#' #Now need to assign another table containing some locations to use the over function
#' 
#' datashield.assign(opals, symbol='locations', value='GEOSPATIAL.GPS_location')
#' 
#' #assign the correct projection to the buffer object
#' ds.proj4string('my_buffer', 29902, 'my_buffer')
#'
#' #Convert the standard data frame to points, set the projection and transform
#' #for the locations
#'
#' ds.coordinates('locations',c('Lon','Lat'), newobj='locations')
#'
#' ds.proj4string('locations', 4326, 'locations')
#' ds.spTransform('locations', 29902, 'locations')
#'
#' #Overlay the buffered regions and locations returning a count of matches
#' #where the locations fall inside the buffer
#' ds.over('my_buffer','locations',fn='length',returnList=F,'res')
#' 
#' }
#' 
ds.over = function(x=NULL, y=NULL, fn=NULL, returnList=FALSE, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of a spatial points object for the geometry of the query!", call.=FALSE)
  }
  
  # check if a valid proj4 epsg coordinate system identifier string has been provided
  if(is.null(y)){
    stop("Please provide the name of a spatial points object for the geometry of the layer
         from which the attributes are queried!", call.=FALSE)
  }

  
  # check if the input object(s) is(are) defined in all the studies
  defined_x <- isDefined(datasources, x)
  defined_y <- isDefined(datasources, y)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  typ2 <- checkClass(datasources, y)
  # if the input object is not a spatial object stop
  if(typ != 'SpatialPointsDataFrame' & typ != 'SpatialPoints' &
       typ != 'SpatialLinesDataFrame' & typ != 'SpatialLines'&
       typ != 'SpatialPolygonsDataFrame' & typ != 'SpatialPolygons'){
    stop("The input object must be of type SpatialXXXXX(DataFrame)", call.=FALSE)
  }
  # if the input object is not a spatial object stop
  if(typ2 != 'SpatialPointsDataFrame' & typ2 != 'SpatialPoints' &
       typ2 != 'SpatialLinesDataFrame' & typ2 != 'SpatialLines'&
       typ2 != 'SpatialPolygonsDataFrame' & typ2 != 'SpatialPolygons'){
    stop("The input object must be of type SpatialXXXXX(DataFrame)", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- paste0(x,".over")
  }
  
  # call the server side function and do the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Overlaying geometries on ", names(datasources)[i], "..."))
    if (!is.null(fn)){
     fnStr <- paste0(", ",fn) 
    }
    else {
      fnStr <- ", NULL"
    }
    cally <- paste0("overDS(", x,",",y,",", returnList, fnStr,")")
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
}
