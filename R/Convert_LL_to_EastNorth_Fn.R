
#' Convert from Lat-Long to Eastings-Northings using WGS
#'
#' \code{Convert_LL_to_UTM_Fn} converts from Latitude-Longitude to World Geodetic System Eastings-Northings for a given location
#'
#' @param Lat vector of latitudes
#' @param Lon vector of longitudes
#' @param crs EPSG reference for coordinate reference system (CRS) defining Eastings-Northings after transformation

#' @return A data frame with the following columns
#' \describe{
#'   \item{X}{The UTM eastings for each value of Lon}
#'   \item{Y}{The UTM northings measured from the equator for each Lat}
#' }

#' @export
Convert_LL_to_EastNorth_Fn <-
function( Lon, Lat, crs=NA ){
  # SEE:  https://github.com/nwfsc-assess/geostatistical_delta-GLMM/issues/25#issuecomment-345825230

  # Attach package
  require(rgdal)
  on.exit( detach("package:rgdal") )

  # Transform
  dstart<-data.frame(lon=Lon, lat=Lat) # that's the object
  coordinates(dstart) <- c("lon", "lat")
  proj4string(dstart) <- CRS("+init=epsg:4326") # that's the lat long projection
  CRS.new <- CRS(crs) # that's the eastings and northings projection
  dstart.t <- spTransform(dstart, CRS.new) # here's where you transform

  # Clean up
  dstart.t = cbind( "E_km"=dstart.t@coords[,"lon"]/1000, "N_km"=dstart.t@coords[,'lat']/1000 )
  attr(dstart.t,"zone") = crs

  # Return results
  return( dstart.t )
}
