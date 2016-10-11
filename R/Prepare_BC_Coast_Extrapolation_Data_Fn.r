#' @export
Prepare_BC_Coast_Extrapolation_Data_Fn <-
function( strata.limits=NULL, strata_to_use=c('SOG','WCVI','QCS','HS','WCHG'), zone=NA, ... ){
  # Infer strata
  if( is.null(strata.limits)){
    strata.limits = data.frame('STRATA'="All_areas")
  }
  message("Using strata ", strata.limits)

  # Read extrapolation data
  utils::data( bc_coast_grid, package="SpatialDeltaGLMM" )
  bc_coast_grid <- cbind( bc_coast_grid, "ALL"=13.74)
  Data_Extrap <- bc_coast_grid

  # Survey areas
  Area_km2_x = rowSums( Data_Extrap[,strata_to_use,drop=FALSE] )
  
  # Augment with strata for each extrapolation cell
  Tmp = cbind("BEST_DEPTH_M"=0, "BEST_LAT_DD"=Data_Extrap[,'Lat'], "BEST_LON_DD"=Data_Extrap[,'Lon'])
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=length(strata.limits), dimnames=list(NULL,names(strata.limits))))
  for(l in 1:ncol(a_el)){
    a_el[,l] = apply(Tmp, MARGIN=1, FUN=SpatialDeltaGLMM:::match_strata_fn, strata_dataframe=strata.limits[l,,drop=FALSE])
    a_el[,l] = ifelse( is.na(a_el[,l]), 0, Area_km2_x)
  }

  # Convert extrapolation-data to an Eastings-Northings coordinate system
  tmpUTM = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=Data_Extrap[,'Lon'], Lat=Data_Extrap[,'Lat'], zone=zone)
  
  # Extra junk
  Data_Extrap = cbind( Data_Extrap, 'Include'=1)
  Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]

  # Return
  Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(tmpUTM,"zone"), "flip_around_dateline"=FALSE, "Area_km2_x"=Area_km2_x)
  return( Return )
}
