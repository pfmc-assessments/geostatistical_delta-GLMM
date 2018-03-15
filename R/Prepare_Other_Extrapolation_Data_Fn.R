#' @export
Prepare_Other_Extrapolation_Data_Fn <-
  function( strata.limits, observations_LL, grid_dim_km=c(2,2), maximum_distance_from_sample=NULL,
            zone=NA, grid_in_UTM=TRUE, grid_dim_LL=c(0.1,0.1), flip_around_dateline=FALSE, ... ){
    
    # Local function
    rename_columns = function( DF, origname=colnames(DF), newname ){
      DF_new = DF
      for(i in 1:length(origname)){
        Match = match( origname[i], colnames(DF_new) )
        if(length(Match)==1) colnames(DF_new)[Match] = newname[i]
      }
      return(DF_new)
    }
    
    if( grid_in_UTM==TRUE ){
      # Fill in missing inputs
      if( is.null(maximum_distance_from_sample) ) maximum_distance_from_sample = sqrt((grid_dim_km[1]/2)^2+(grid_dim_km[2]/2)^2)
      
      #One hemisphere calculations
      if(max(observations_LL[, "Lat"]) < 0 | min(observations_LL[, "Lat"]) > 0){
        print('data are in the one hemisphere')
        observations_UTM = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=observations_LL[,'Lon'], Lat=observations_LL[,'Lat'], zone=zone, flip_around_dateline=flip_around_dateline)
        E_lim = mean(range(observations_UTM[,'X'])) + c(-0.6,0.6)*diff(range(observations_UTM[,'X']))
        N_lim = mean(range(observations_UTM[,'Y'])) + c(-0.6,0.6)*diff(range(observations_UTM[,'Y']))
        
        # Specify if northern or southern
        
        # Make grid
        Data_Extrap = expand.grid( "E_km"=seq(E_lim[1],E_lim[2],by=grid_dim_km[1]), "N_km"=seq(N_lim[1],N_lim[2],by=grid_dim_km[2]), "Area_km2"=prod(grid_dim_km) )
        
        # Add LL
        TmpUTM = rename_columns( Data_Extrap[,c("E_km","N_km")], newname=c("X","Y"))
        attr(TmpUTM, "projection") = "UTM"
        attr(TmpUTM, "zone") = attr(observations_UTM,"zone")
        
        if(max(observations_LL[, "Lat"]) < 0) TmpLL = PBSmapping::convUL(TmpUTM, southern=TRUE )
        if(min(observations_LL[, "Lat"]) > 0) TmpLL = PBSmapping::convUL(TmpUTM, southern=FALSE )
        
        Data_Extrap = cbind( Data_Extrap, rename_columns(TmpLL,newname=c("Lon","Lat")) )
        
        # Restrict to grid locations near samples
        NN_Extrap = RANN::nn2( query=Data_Extrap[,c("E_km","N_km")], data=observations_UTM[,c("X","Y")], k=1)
        Data_Extrap = cbind( Data_Extrap, "Include"=ifelse(NN_Extrap$nn.dists<maximum_distance_from_sample,1,0))
        
        # Survey areas
        Area_km2_x = Data_Extrap[,'Area_km2'] * Data_Extrap[,'Include']
      }
      
      #Both hemispheres
      if(min(observations_LL[, 'Lat']) < 0 & max(observations_LL[, 'Lat']) > 0){
        print("data span northern and southern hemispheres")
        
        #------------------------------------------------
        #Do north
        observations_LL_n <- subset(observations_LL, Lat > 0)
        
        observations_UTM_n = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=observations_LL_n[,'Lon'], Lat=observations_LL_n[,'Lat'], zone=zone, flip_around_dateline=flip_around_dateline)
        E_lim_n = mean(range(observations_UTM_n[,'X'])) + c(-0.6,0.6)*diff(range(observations_UTM_n[,'X']))
        N_lim_n = mean(range(observations_UTM_n[,'Y'])) + c(-0.6,0.6)*diff(range(observations_UTM_n[,'Y']))
        
        Data_Extrap_n = expand.grid( "E_km"=seq(E_lim_n[1],E_lim_n[2],by=grid_dim_km[1]), "N_km"=seq(N_lim_n[1],N_lim_n[2],by=grid_dim_km[2]), "Area_km2"=prod(grid_dim_km) )
        
        # Add LL
        TmpUTM_n = rename_columns( Data_Extrap_n[,c("E_km","N_km")], newname=c("X","Y"))
        attr(TmpUTM_n, "projection") = "UTM"
        attr(TmpUTM_n, "zone") = attr(observations_UTM_n,"zone")
        
        TmpLL_n = PBSmapping::convUL(TmpUTM_n, southern=FALSE)
        Data_Extrap_n = cbind( Data_Extrap_n, rename_columns(TmpLL_n,newname=c("Lon","Lat")) )
        
        # Restrict to grid locations near samples
        NN_Extrap_n = RANN::nn2( query=Data_Extrap_n[,c("E_km","N_km")], data=observations_UTM_n[,c("X","Y")], k=1)
        Data_Extrap_n = cbind( Data_Extrap_n, "Include"=ifelse(NN_Extrap_n$nn.dists<maximum_distance_from_sample,1,0))
        
        # Survey areas
        Area_km2_x_n = Data_Extrap_n[,'Area_km2'] * Data_Extrap_n[,'Include']
        
        #------------------------------------------------
        #Do south
        observations_LL_s <- subset(observations_LL, Lat < 0)
        
        observations_UTM_s = SpatialDeltaGLMM::Convert_LL_to_UTM_Fn( Lon=observations_LL_s[,'Lon'], Lat=observations_LL_s[,'Lat'], zone=zone, flip_around_dateline=flip_around_dateline)
        E_lim_s = mean(range(observations_UTM_s[,'X'])) + c(-0.6,0.6)*diff(range(observations_UTM_s[,'X']))
        N_lim_s = mean(range(observations_UTM_s[,'Y'])) + c(-0.6,0.6)*diff(range(observations_UTM_s[,'Y']))
        
        Data_Extrap_s = expand.grid( "E_km"=seq(E_lim_s[1],E_lim_s[2],by=grid_dim_km[1]), "N_km"=seq(N_lim_s[1],N_lim_s[2],by=grid_dim_km[2]), "Area_km2"=prod(grid_dim_km) )
        
        # Add LL
        TmpUTM_s = rename_columns( Data_Extrap_s[,c("E_km","N_km")], newname=c("X","Y"))
        attr(TmpUTM_s, "projection") = "UTM"
        attr(TmpUTM_s, "zone") = attr(observations_UTM_s,"zone")
        
        TmpLL_s = PBSmapping::convUL(TmpUTM_s, southern=TRUE)
        Data_Extrap_s = cbind( Data_Extrap_s, rename_columns(TmpLL_s,newname=c("Lon","Lat")) )
        
        # Restrict to grid locations near samples
        NN_Extrap_s = RANN::nn2( query=Data_Extrap_s[,c("E_km","N_km")], data=observations_UTM_s[,c("X","Y")], k=1)
        Data_Extrap_s = cbind( Data_Extrap_s, "Include"=ifelse(NN_Extrap_s$nn.dists<maximum_distance_from_sample,1,0))
        
        # Survey areas
        Area_km2_x_s = Data_Extrap_s[,'Area_km2'] * Data_Extrap_s[,'Include']
        
        #------------------------------------------------
        # Combine the north and south data types
        #Adjust the southern hemisphere northings
        Data_Extrap_s$N_km <- Data_Extrap_s$N_km - 10000
        
        Data_Extrap <- rbind(Data_Extrap_n, Data_Extrap_s)
        Area_km2_x <- c(Area_km2_x_n, Area_km2_x_s)
        
        #Combine TmpUTM
        TmpUTM <- rbind(TmpUTM_n, TmpUTM_s)
      }
    }else{
      # Fill in missing inputs
      if( is.null(maximum_distance_from_sample) ) maximum_distance_from_sample = sqrt((grid_dim_LL[1]/2)^2+(grid_dim_LL[2]/2)^2)
      
      # Get range
      Lat_lim = mean(range(observations_LL[,'Lat'])) + c(-0.6,0.6)*diff(range(observations_LL[,'Lat']))
      if( flip_around_dateline==FALSE ) Lon_lim = mean(range(observations_LL[,'Lon'])) + c(-0.6,0.6)*diff(range(observations_LL[,'Lon']))
      if( flip_around_dateline==TRUE ){
        Tmp_Lon = 180 + ifelse( observations_LL[,'Lon']>0, observations_LL[,'Lon']-360, observations_LL[,'Lon'])
        Lon_lim = mean(range(Tmp_Lon)) + c(-0.6,0.6)*diff(range(Tmp_Lon))
      }
      
      # Make grid
      Data_Extrap = expand.grid( "Lon"=seq(Lon_lim[1],Lon_lim[2],by=grid_dim_LL[1]), "Lat"=seq(Lat_lim[1],Lat_lim[2],by=grid_dim_LL[2]), "Area_km2"=110^2*prod(grid_dim_LL) )
      if( flip_around_dateline==TRUE ){
        Data_Extrap[,'Lon'] = ifelse( Data_Extrap[,'Lon']<0, Data_Extrap[,'Lon']+360, Data_Extrap[,'Lon'] ) - 180
      }
      
      # Add empty UTM
      Data_Extrap = cbind( Data_Extrap, "E_km"=NA, "N_km"=NA, "Area_km2"=prod(grid_dim_km) )
      TmpUTM = rename_columns( Data_Extrap[,c("E_km","N_km")], newname=c("X","Y"))
      TmpUTM = NA
      attr(TmpUTM, "zone") = NA
      
      # Restrict to grid locations near samples
      NN_Extrap = RANN::nn2( query=Data_Extrap[,c("Lat","Lon")], data=observations_LL[,c('Lat','Lon')], k=1)
      Data_Extrap = cbind( Data_Extrap, "Include"=ifelse(NN_Extrap$nn.dists<maximum_distance_from_sample,1,0))
      
      # Survey areas
      Area_km2_x = Data_Extrap[,'Area_km2'] * Data_Extrap[,'Include']
    }
    
    # Augment with strata for each extrapolation cell
    Tmp = cbind("BEST_DEPTH_M"=0, "BEST_LAT_DD"=Data_Extrap[,'Lat'], "BEST_LON_DD"=Data_Extrap[,'Lon'])
    a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=nrow(strata.limits), dimnames=list(NULL,strata.limits[,'STRATA'])))
    for(l in 1:ncol(a_el)){
      a_el[,l] = apply(Tmp, MARGIN=1, FUN=SpatialDeltaGLMM::match_strata_fn, strata_dataframe=strata.limits[l,,drop=FALSE])
      a_el[,l] = ifelse( is.na(a_el[,l]), 0, Area_km2_x)
    }
    
    # Return
    Return = list( "a_el"=a_el, "Data_Extrap"=Data_Extrap, "zone"=attr(TmpUTM,"zone"), "flip_around_dateline"=flip_around_dateline, "Area_km2_x"=Area_km2_x)
    return( Return )
    
  }
