#' 
#' @title function to indicate when an individual is moving between two locations
#' @description This function is used to determine whether an individual is moving between
#' two locations, for example when commuting between home and work.
#' @details Several assumptions are made about the nature of the data being examined.
#'  It is assumed that some groundwork has been done to provide a set of flags
#'  to indicate when the individual is at either of the locations. This data is grouped per
#'   individual, the groupings are defined by an identifier column. It is most likely that
#'   the input data will be a SpatialPointsDataFrame. The points making up an individual's
#'  journey should be temporally ordered. The function is able to detect when the individual
#'   leaves one location and arrives at the other and vice versa. Journeys that start and
#'   arrive at the same location are not counted.
#' @param input name of the object (usually SpatialPointsDataFrame) which contains the 
#' points of the individuals' journeys and the two locations of interest
#' @param id_col name of the column in the data frame that identifies which points belong to
#' each individual
#' @param loc1_col name of the column in the data frame that indicates when the individual
#' is at the first location
#' @param loc2_col name of the column in the data frame that indicates when the individual
#' is at the second location
#' @param newobj a character, the name of the new object which will be created
#' If no name is specified the default name is the name of the original data frame 
#' followed by the suffix '.over'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a vector of 1s and 0s, with a 1 indicating that the individual was travelling
#' between the two locations
#' @author Bishop, T.
#' @export
#' @examples {
#' 
#' 
#' #' # Load log in data
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
#' # making the journey are at their own home or work and not any one else's.
#' # Thus the output vectors contain an indicator showing whether the individual
#' # was at home or work
#'
#' ds.over('journeys', 'home_buffer',returnList=TRUE,newobj='home_over')
#' ds.overMatch('journeys','person_id','home_over','person_id',newobj='res_home')
#' 
#' ds.over('journeys', 'work_buffer',returnList=TRUE,newobj='work_over')
#' ds.overMatch('journeys','person_id','work_over','person_id',newobj='res_work')
#'
#' # Create a data frame with the data created above, with the indicator for the
#' # individual's home and work. These data are grouped by the person_id column
#'   
#' ds.dataframe(x=c('D$person_id','res_home','res_work'),'commute_input')
#' 
#' # The commute function determines whether a journeys are between two locations
#' # that are indicated by two columns in the input dataframe. In this case the
#' # locations are the home and work of several individuals. The output is a vector
#' # of indicators showing that the individual is travelling between the two
#' # locations of interest
#' 
#' ds.commute('commute_input', 'person_id', 'res_home', 'res_work', 'commute_out')
#' 
#' }
#' 
ds.commute = function(input, id_col, loc1_col, loc2_col, newobj=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(input)){
    stop("Please provide the name of an object that contains the individuals' journeys", call.=FALSE)
  }

  if(is.null(id_col)){
    stop("Please provide the name of the column in the input object that identifies individuals", call.=FALSE)
  }
  
  if(is.null(loc1_col)){
    stop("Please provide the name of an object that contains the individuals' journeys", call.=FALSE)
  }
  
  if(is.null(loc2_col)){
    stop("Please provide the name of an object that contains the individuals' journeys", call.=FALSE)
  }
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, input)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, input)
  # if the input object is not a spatial object stop
  if(typ != 'SpatialPointsDataFrame' & typ != 'data.frame'){
    stop("The input object must be of type SpatialPointsDataFrame or a data frame", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- paste0(input,".comm")
  }
  
  # call the server side function and do the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Calculating commutes for ", names(datasources)[i], "..."))
    cally <- paste0("commuteDS(", input, ",'",id_col,"','",loc1_col,"','",loc2_col,"')")
    datashield.assign(datasources[i], newobj, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources[i], newobj) 
    
  }
  
  }
