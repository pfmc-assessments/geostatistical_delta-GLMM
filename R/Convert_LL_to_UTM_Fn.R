Convert_LL_to_UTM_Fn <-
function( Lon, Lat, zone=NA, flip_around_dateline=FALSE ){

  # Convert
  # if zone=NA or NULL, then it automatically detects appropriate zone
  Tmp = cbind('PID'=1,'POS'=1:length(Lon),'X'=Lon,'Y'=Lat)
  if( flip_around_dateline==TRUE ) Tmp['Lon'] = 180 + ifelse( Tmp['Lon']>0, Tmp['Lon']-360, Tmp['Lon'])
  attr(Tmp,"projection") = "LL"
  attr(Tmp,"zone") = zone
  tmpUTM = convUL(Tmp)                                                         #$

  # Return results
  return( tmpUTM )
}
