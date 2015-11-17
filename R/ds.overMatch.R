#' 
#' @title Function to compliment \code{ds.over()} function
#' @description This function is used in conjunction with the \code{over()} function.
#' When the two sets of objects that have been overlaid and those objects share a common
#' feature, validate that where there is an overlay, the attribute matches. Return a 
#' vector indicating where a match is seen.
#' @details The details are best explained through an example. If we have a group of
#' individuals with many points representing their journeys, we can attribute their
#' ID to each point and group their points in this way as SpatialPointsDataFrames.
#' We also have another set of polygons, one for each individual, marked with their
#' ID. When we run the \code{ds.over()} function with the points and polygons, this
#' will return all overlays of points and polygons (as a list of dataframes). We might
#' only be interested when an individual's points fall inside the polygon marked with
#' their own ID and not the ID of others. In this case this function will return
#' a vector indicating which of an individual's points fell inside their polygon.
#' @param x spatial object that was previously used as geometry (locations) of 
#' the queries when using \code{ds.over()}
#' @param x_id the name of the attribute of interest on x
#' @param over_out output from previous use of \code{ds.over()} where x was queried
#' @param y_id the name of the attribute of interest on y that was used previously
#' in \code{ds.over()}
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.overM'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a vector indicating a positive match
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
#' # set up additional objects needed for the example
#' 
#' datashield.assign(opals, symbol="work", value="GEOSPATIAL.GPS_work")
#' datashield.assign(opals, symbol="home", value="GEOSPATIAL.GPS_home")
#' 
#' # Convert points data to a SpatialPointsDataFrame
#' 
#' ds.coordinates('D',coords=c('Lon','Lat'),newobj='journeys')
#' ds.coordinates('work',coords=c('Lon','Lat'),newobj='work')
#' ds.coordinates('home',coords=c('Lon','Lat'),newobj='home')
#'
#' #set up coordinate systems
#'
#' ds.proj4string('journeys',4326,'journeys')
#' ds.proj4string('work',4326,'work')
#' ds.proj4string('home',4326,'home')
#' 
#' ds.spTransform('journeys',29902,'journeys')
#' ds.spTransform('work',29902,'work')
#' ds.spTransform('home',29902,'home')
#' 
#' #Create buffer around point locations
#'
#' ds.gBuffer('work',by_id=TRUE,ip_width=150,'work_buffer')
#' ds.gBuffer('home',by_id=TRUE,ip_width=150,'home_buffer')
#'
#' # Overlay the locations with the journeys with the home and work locations.
#' # We then use the overMatch function to only count matches when the ID of the
#' # location and journey are the same so that we only consider when individuals
#' # making the journey are at their own home or work and not any one else's
#'
#' ds.over('journeys', 'home_buffer',returnList=TRUE,newobj='home_over')
#' ds.overMatch('journeys','person_id','home_over','person_id',newobj='res_home')
#' 
#' ds.over('journeys', 'work_buffer',returnList=TRUE,newobj='work_over')
#' ds.overMatch('journeys','person_id','work_over','person_id',newobj='res_work')
#' 
#' }

ds.overMatch = function(x=NULL, x_id=NULL, over_out=NULL, y_id=NULL,
                         newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the spatial object x that you wish to match the
         overlaid list over_out to", call.=FALSE)
  }
  
  if(is.null(x_id)){
    stop("Please provide the name of the identifying column in the data frame of the
         spatial object x that was used to generate the overlay list", call.=FALSE)
  }

  if(is.null(over_out)){
    stop("Please provide the name of the list of overlaid geometries that was
         generated from the spatial object x and another spatial object y with matching
         identifiers", call.=FALSE)
  }
  
  if(is.null(y_id)){
    stop("Please provide the name of the identifying column in the data frame of the
         spatial object y that was used to generate the overlay list", call.=FALSE)
  }
  
  
  # check if the input object(s) is(are) defined in all the studies
  defined_x <- isDefined(datasources, x)
  defined_over_out <- isDefined(datasources, over_out)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  typ2 <- checkClass(datasources, over_out)
  # if the input object is not a matrix or a dataframe stop
  #   if(typ != 'SpatialPointsDataFrame' & typ != 'SpatialPoints'){
  #     stop("The input vector must be of type 'SpatialPointsDataFrame'!", call.=FALSE)
  #   }
  
  if(is.null(newobj)){
    newobj <- paste0(x,".overM")
  }
  
  # call the server side function and do the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Matching overlay on ", names(datasources)[i], "..."))
    cally <- paste0("overMatchDS(", x,",'",x_id,"',", over_out,",'", y_id,"')")
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
  }
