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
#' # bus_out is a SpatialPointsDataFrame with 2 columns in the data frame called
#' # res_home and res_work representing home and work. These columns are 1 or 0 
#' # depending on whether the individual is at that location or not. The data
#' # also contains many rows (i.e. points in a journey) per individual which are
#' # grouped by the bus_id column.
#' 
#' ds.commute('bus_out', 'bus_id', 'res_home', 'res_work', 'bus_comm')
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
