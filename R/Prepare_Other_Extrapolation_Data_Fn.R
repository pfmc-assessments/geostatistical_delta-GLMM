#' @export
Prepare_Other_Extrapolation_Data_Fn <-
function( strata.limits, observations_LL, grid_dim_km=c(2,2), maximum_distance_from_sample=sqrt((grid_dim_km[1]/2)^2+(grid_dim_km[2]/2)^2), zone=NA, ... ){
  # Local function
  rename_columns = function( DF, origname=colnames(DF), newname ){
    DF_new = DF
    for(i in 1:length(origname)){
      Match = match( origname[i], colnames(DF_new) )
      if(length(Match)==1) colnames(DF_new)[Match] = newname[i]
    }
    return(DF_new)
  }

  # Get range
  observations_UTM = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=observations_LL[,'Lon'], Lat=observations_LL[,'Lat'], zone=zone)                                                         #$
  E_lim = mean(range(observations_UTM[,'X'])) + c(-0.6,0.6)*diff(range(observations_UTM[,'X']))
  N_lim = mean(range(observations_UTM[,'Y'])) + c(-0.6,0.6)*diff(range(observations_UTM[,'Y']))

  # Make grid
  Data_Extrap = expand.grid( "E_km"=seq(E_lim[1],E_lim[2],by=grid_dim_km[1]), "N_km"=seq(N_lim[1],N_lim[2],by=grid_dim_km[2]), "Area_km2"=prod(grid_dim_km) )

  # Add LL
  TmpUTM = rename_columns( Data_Extrap[,c("E_km","N_km")], newname=c("X","Y"))
  attr(TmpUTM, "projection") = "UTM"
  attr(TmpUTM, "zone") = attr(observations_UTM,"zone")
  TmpLL = PBSmapping::convUL(TmpUTM)
  Data_Extrap = cbind( Data_Extrap, rename_columns(TmpLL,newname=c("Lon","Lat")) )

  # Restrict to grid locations near samples
  NN_Extrap = RANN::nn2( query=Data_Extrap[,c("E_km","N_km")], data=observations_UTM[,c("X","Y")], k=1)
  Data_Extrap = cbind( Data_Extrap, "Include"=ifelse(NN_Extrap$nn.dists<maximum_distance_from_sample,1,0))

  # Survey areas
  Area_km2_x = Data_Extrap[,'Area_km2'] * Data_Extrap[,'Include']
  
  # Augment with strata for each extrapolation cell
  Tmp = cbind("BEST_DEPTH_M"=0, "BEST_LAT_DD"=Data_Extrap[,'Lat'], "BEST_LON_DD"=Data_Extrap[,'Lon'])
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=nrow(strata.limits), dimnames=list(NULL,strata.limits[,'STRATA'])))
  for(l in 1:ncol(a_el)){
    a_el[,l] = apply(Tmp, MARGIN=1, FUN=SpatialDeltaGLMM:::match_strata_fn, strata_dataframe=strata.limits[l,,drop=FALSE])
    a_el[,l] = ifelse( is.na(a_el[,l]), 0, Area_km2_x)
  }

  # Return
  Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(TmpUTM,"zone"), "flip_around_dateline"=FALSE, "Area_km2_x"=Area_km2_x)
  return( Return )
}
