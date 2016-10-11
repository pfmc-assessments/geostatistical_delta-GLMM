#' @export
Prepare_GOA_Extrapolation_Data_Fn <-
function( strata.limits=NULL, zone=NA, ... ){
  # Infer strata
  if( is.null(strata.limits)){
    strata.limits = data.frame('STRATA'="All_areas")
  }
  message("Using strata ", strata.limits)

  # Read extrapolation data
  utils::data( gulf_of_alaska_grid, package="SpatialDeltaGLMM" )
  Data_Extrap <- gulf_of_alaska_grid

  # Survey areas
  #Area_Tmp = 4 * 1.852^2 * ifelse( Data_Extrap[,'EBS_STRATUM']!=0, 1, 0 )
  Area_km2_x = Data_Extrap[,'Area_in_survey_km2']
  
  # Augment with strata for each extrapolation cell
  Tmp = cbind("BEST_DEPTH_M"=0, "BEST_LAT_DD"=Data_Extrap[,'Lat'], "BEST_LON_DD"=Data_Extrap[,'Lon'], "propInSurvey"=1)
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=nrow(strata.limits), dimnames=list(NULL,strata.limits[,'STRATA'])))
  for(l in 1:ncol(a_el)){
    a_el[,l] = apply(Tmp, MARGIN=1, FUN=SpatialDeltaGLMM:::match_strata_fn, strata_dataframe=strata.limits[l,,drop=FALSE])
    a_el[,l] = ifelse( is.na(a_el[,l]), 0, Area_km2_x)
  }

  # Convert extrapolation-data to an Eastings-Northings coordinate system
  #Data_Extrap[,'Lon'] = 180 + ifelse( Data_Extrap[,'Lon']>0, Data_Extrap[,'Lon']-360, Data_Extrap[,'Lon'])
  tmpUTM = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=Data_Extrap[,'Lon'], Lat=Data_Extrap[,'Lat'], zone=zone, flip_around_dateline=FALSE)                                                         #$
  #Data_Extrap[,'Lon'] = -180 + ifelse( Data_Extrap[,'Lon']<0, Data_Extrap[,'Lon']+360, Data_Extrap[,'Lon'])
  
  # Extra junk
  Data_Extrap = cbind( Data_Extrap, 'Include'=1)
  Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]

  # Return
  Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(tmpUTM,"zone"), "flip_around_dateline"=FALSE, "Area_km2_x"=Area_km2_x)
  return( Return )
}
