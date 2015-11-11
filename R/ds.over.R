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
#' # Return the points of interest that fall within each region in a new object 'res'
#' # 'my_regions' consists of several regions defined in a SpatialPolygonDataFrame
#' # 'my_poi' consists of points of interest in a SpatialPointsDataFrame
#' 
#' ds.over(x='my_regions',y='my_poi',newobj='res')
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
