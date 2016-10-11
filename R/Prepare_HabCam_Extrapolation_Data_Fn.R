Prepare_HabCam_Extrapolation_Data_Fn <-
function( strata.limits=NULL, zone=NA, ... ){
  # Infer strata
  if( is.null(strata.limits)){
    strata.limits = data.frame('STRATA'="All_areas")
  }
  message("Using strata ", strata.limits)

  # Read extrapolation data
  utils::data( habcam_grid, package="SpatialDeltaGLMM" )
  #load( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/data/habcam_grid.rda" )
  Data_Extrap <- habcam_grid

  # Survey areas
  Area_km2_x = Data_Extrap[,'Cell_area_km2']
  
  # Augment with strata for each extrapolation cell
  Tmp = cbind("Longitude"=Data_Extrap[,'Lon'], "BEST_LAT_DD"=Data_Extrap[,'Lat'], "propInSurvey"=1)
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=nrow(strata.limits), dimnames=list(NULL,strata.limits[,'STRATA'])))
  for(l in 1:ncol(a_el)){
    a_el[,l] = apply(Tmp, MARGIN=1, FUN=SpatialDeltaGLMM:::match_strata_fn, strata_dataframe=strata.limits[l,,drop=FALSE])
    a_el[,l] = ifelse( is.na(a_el[,l]), 0, Area_km2_x)
  }
             #
  # Convert extrapolation-data to an Eastings-Northings coordinate system
  tmpUTM = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=Data_Extrap[,'Lon'], Lat=Data_Extrap[,'Lat'], zone=zone, flip_around_dateline=TRUE)                                                         #$
           #

  # Extra junk
  Data_Extrap = cbind( Data_Extrap, 'Include'=1)
  Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]

  # Return
  Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(tmpUTM,"zone"), "flip_around_dateline"=TRUE, "Area_km2_x"=Area_km2_x)
  return( Return )
}
