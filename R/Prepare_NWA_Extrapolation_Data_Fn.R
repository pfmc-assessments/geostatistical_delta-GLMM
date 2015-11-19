Prepare_NWA_Extrapolation_Data_Fn <-
function( strata.limits ){
  # Read extrapolation data
  data( northwest_atlantic_grid )
  Data_Extrap <- northwest_atlantic_grid

  # Survey areas
  Area_Tmp = Data_Extrap[,'Area_in_survey_km2']
  
  # Augment with strata for each extrapolation cell
  Tmp = cbind("BEST_DEPTH_M"=0, "BEST_LAT_DD"=Data_Extrap[,'Lat'], "BEST_LON_DD"=Data_Extrap[,'Lon'])
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=length(strata.limits), dimnames=list(NULL,names(strata.limits))))
  for(l in 1:ncol(a_el)){
    a_el[,l] = ifelse( Data_Extrap[,'stratum_number'] %in% strata.limits[[l]], Area_Tmp, 0 )
  }

  # Convert extrapolation-data to an Eastings-Northings coordinate system
  tmpUTM = Convert_LL_to_UTM_Fn( Lon=Data_Extrap[,'Lon'], Lat=Data_Extrap[,'Lat'], zone=NA)                                                         
  
  # Extra junk
  Data_Extrap = cbind( Data_Extrap, 'Include'=1)
  Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]

  # Return
  Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(tmpUTM,"zone"), "flip_around_dateline"=flip_around_dateline)
  return( Return )
}
