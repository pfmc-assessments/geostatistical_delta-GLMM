
#' Convert from Lat-Long to UTM
#'
#' \code{Convert_LL_to_UTM_Fn} converts from Latitude-Longitude to Universal Transverse Mercator projections for a given location
#'
#' @param Lat vector of latitudes
#' @param Lon vector of longitudes
#' @param zone zone used for UTM projection
#' @param flip_around_dateline boolean specifying whether to flip Lat-Lon locations around the dateline, and then retransform back (only useful if Lat-Lon straddle the dateline)

#' @return A data frame with the following columns
#' \describe{
#'   \item{X}{The UTM eastings for each value of Lon}
#'   \item{Y}{The UTM northings measured from the equator for each Lat}
#' }

#' @export
Convert_LL_to_UTM_Fn <-
function( Lon, Lat, zone=NA, flip_around_dateline=FALSE ){

  # Convert
  # if zone=NA or NULL, then it automatically detects appropriate zone
  Tmp = cbind('PID'=1,'POS'=1:length(Lon),'X'=Lon,'Y'=Lat)
  if( flip_around_dateline==TRUE ) Tmp[,'X'] = 180 + ifelse( Tmp[,'X']>0, Tmp[,'X']-360, Tmp[,'X'])
  attr(Tmp,"projection") = "LL"
  attr(Tmp,"zone") = zone
  tmpUTM = PBSmapping::convUL(Tmp)                                                         #$
  if( !is.na(zone)) message("convUL: For the UTM conversion, used zone ",zone," as specified")

  # Return results
  return( tmpUTM )
}
