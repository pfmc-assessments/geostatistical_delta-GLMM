#' Convert from Lat-Long to Eastings-Northings using WGS
#'
#' \code{Convert_LL_to_EastNorth_Fn} converts from Latitude-Longitude to World Geodetic System Eastings-Northings for a given location
#'
#' @param Lat vector of latitudes
#' @param Lon vector of longitudes
#' @param crs EPSG reference for coordinate reference system (CRS) defining Eastings-Northings after transformation
#' @author James Thorson, Sophie Mormede
#' 
#' @return A data frame with the following columns
#' \describe{
#'   \item{E_km}{The eastings for each value of Lon (in kilometers)}
#'   \item{N_km}{The northings for each value of Lat (in kilometers)}
#' }

#' @export
Convert_LL_to_EastNorth_Fn <-
function( Lon, Lat, crs=NA ){
  # SEE:  https://github.com/nwfsc-assess/geostatistical_delta-GLMM/issues/25#issuecomment-345825230
  # SEE: https://github.com/nwfsc-assess/geostatistical_delta-GLMM/issues/38
  
  # Attach package
  require(sf)
  on.exit( detach("package:sf") )
  
  # Transform
  dstart<-data.frame(lon=Lon, lat=Lat) # that's the object
  dstart<-as.matrix(dstart)
  
  dstart <- st_multipoint(dstart)
  dstart <- st_sfc(dstart, crs = 4326)
  dstart.t<- st_transform(dstart, crs)
  dstart.t<-st_coordinates(dstart.t)
  
  # Clean up
  dstart.t = cbind( "E_km"=dstart.t[,1]/1000, "N_km"=dstart.t[,2]/1000 )
  attr(dstart.t,"zone") = crs
  
  # Return results
  return( dstart.t )
}
