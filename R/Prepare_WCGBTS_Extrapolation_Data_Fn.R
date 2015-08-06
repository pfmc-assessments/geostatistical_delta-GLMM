Prepare_WCGBTS_data_Fn <-
function( strata.limits ){
  # Read extrapolation data
  data( extrapolation_data )
  Data_Extrap <- extrapolation_data

  # Augment with strata for each extrapolation cell
  Tmp = cbind("BEST_DEPTH_M"=(-1000)*Data_Extrap[,'Depth_km'], "BEST_LAT_DD"=Data_Extrap[,'Lat'], "propInWCGBTS"=Data_Extrap[,'propInWCGBTS'])
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=nrow(strata.limits), dimnames=list(NULL,strata.limits[,'STRATA'])))
  for(l in 1:ncol(a_el)){
    a_el[,l] = apply(Tmp , MARGIN=1, FUN=nwfscDeltaGLM::strata.fn, Strata.df=strata.limits[l,])
    a_el[,l] = ifelse( is.na(a_el[,l]), 0, 4*Data_Extrap[,'propInWCGBTS'])
  }

  # Convert extrapolation-data to an Eastings-Northings coordinate system
  tmpUTM = Convert_LL_to_UTM_Fn( Lon=Data_Extrap[,'Lon'], Lat=Data_Extrap[,'Lat'])                                                         #$
  Data_Extrap = cbind( Data_Extrap, 'Include'=(Data_Extrap[,'Cowcod']==0 & Data_Extrap[,'Ngdc_m']<(-35)))
  Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]

  # Return
  Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap)
  return( Return )
}
