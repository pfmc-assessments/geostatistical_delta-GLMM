#' @export
Prepare_SA_Extrapolation_Data_Fn <-
function( strata.limits=NULL, region=c("south_coast","west_coast"), zone=NA, ... ){
  # Infer strata
  if( is.null(strata.limits)){
    strata.limits = data.frame('STRATA'="All_areas")
  }
  message("Using strata ", strata.limits)

  # Read extrapolation data
  utils::data( south_africa_grid, package="SpatialDeltaGLMM" )
  Data_Extrap <- south_africa_grid

  # Survey areas
  Area_km2_x = Data_Extrap[,'nm2'] * 1.852^2 * ifelse( Data_Extrap[,'stratum']%in%region, 1, 0 ) # Convert from nm^2 to km^2
  
  # Augment with strata for each extrapolation cell
  Tmp = cbind("BEST_DEPTH_M"=0, "BEST_LAT_DD"=Data_Extrap[,'cen_lat'], "BEST_LON_DD"=Data_Extrap[,'cen_long'])
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=length(strata.limits), dimnames=list(NULL,names(strata.limits))))
  for(l in 1:ncol(a_el)){
    a_el[,l] = apply(Tmp, MARGIN=1, FUN=SpatialDeltaGLMM:::match_strata_fn, strata_dataframe=strata.limits[l,,drop=FALSE])
    a_el[,l] = ifelse( is.na(a_el[,l]), 0, Area_km2_x)
  }

  # Convert extrapolation-data to an Eastings-Northings coordinate system
  tmpUTM = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=Data_Extrap[,'cen_long'], Lat=Data_Extrap[,'cen_lat'], zone=zone)
  
  # Extra junk
  Data_Extrap = cbind( Data_Extrap, 'Include'=1)
  Data_Extrap[,c('E_km','N_km')] = tmpUTM[,c('X','Y')]
  colnames(Data_Extrap)[1:2] = c("Lon","Lat")

  # Return
  Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(tmpUTM,"zone"), "flip_around_dateline"=FALSE, "Area_km2_x"=Area_km2_x)
  return( Return )
}
