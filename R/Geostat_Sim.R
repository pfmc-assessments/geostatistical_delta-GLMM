
#' @export
Geostat_Sim <-
function(Sim_Settings, Extrapolation_List, Data_Geostat=NULL, MakePlot=FALSE, DateFile=paste0(getwd(),"/"), standardize_fields=FALSE ){
  # Terminology
  # O: Spatial component; E: spatiotemporal component
  # O1: presence-absence; O2: positive catch rate
  # P1 : linear predictor of presence/absence in link-space
  # R1: predictor of presence/absence in natural-space (transformed out of link-space)
  # v_i: vessel for sample i
  # t_i: year for sample i

  # Specify default values
  Settings = list("beta1_mean"=0, "beta2_mean"=0, "beta1_slope"=0, "beta2_slope"=0, "beta1_sd"=0, "beta2_sd"=0, "Nyears"=10, "Nsamp_per_year"=600, "Depth_km"=0,
    "Depth_km2"=0, "Dist_sqrtkm"=0, "SigmaO1"=0.5, "SigmaO2"=0.5, "SigmaE1"=0.1, "SigmaE2"=0.1, "SigmaV1"=0, "SigmaV2"=0, "SigmaVY1"=0, "SigmaVY2"=0,
    "Range1"=1000, "Range2"=500, "SigmaM"=1, "ObsModel"=c(2,0) )
  # Replace defaults with provided values (if any)
  for( i in 1:length(Sim_Settings)){
    if(names(Sim_Settings)[i] %in% names(Settings)){
      Settings[[match(names(Sim_Settings)[i],names(Settings))]] = Sim_Settings[[i]]
    }
  }

  # Attach stuff
  start_time = Sys.time()
  library( maps )
  library( mapdata )
  on.exit( detach(package:mapdata), add=TRUE )
  on.exit( detach(package:maps), add=TRUE )

  # Local functions
  RFsim = function(model, x, y, standardize=TRUE){
    if( model_O1@par.general$var>0 ){
      vec = RandomFields::RFsimulate(model=model, x=x, y=y)@data[,1]
      if(standardize==TRUE) vec = (vec - mean(vec)) / sd(vec) * sqrt(model@par.general$var)
    }else{
      vec = rep(0,length(x))
    }
    return(vec)
  }
  rPoisGam = function( n=1, shape, scale, intensity ){
    Num = rpois( n=n, lambda=intensity )
    Biomass = rep(0, length=n)
    for(i in which(Num>0) ) Biomass[i] = sum( rgamma(n=Num[i], shape=shape, scale=scale) )
    return( Biomass )
  }

  # Initialize random-field objects
  model_O1 = RandomFields::RMgauss(var=Settings[['SigmaO1']]^2, scale=Settings[['Range1']])
  model_E1 = RandomFields::RMgauss(var=Settings[['SigmaE1']]^2, scale=Settings[['Range1']])
  model_O2 = RandomFields::RMgauss(var=Settings[['SigmaO2']]^2, scale=Settings[['Range2']])
  model_E2 = RandomFields::RMgauss(var=Settings[['SigmaE2']]^2, scale=Settings[['Range2']])

  # Define sampling locations
  if( is.null(Data_Geostat) ){
    s_i = sample(1:nrow(Extrapolation_List$Data_Extrap), size=Settings[['Nyears']]*Settings[['Nsamp_per_year']]) #, nrow=Settings[['Nsamp_per_year']], ncol=Settings[['Nyears']])
    t_i = rep( 1:Settings[['Nyears']], each=Settings[['Nsamp_per_year']])
    v_i = rep(rep( 1:4, each=Settings[['Nsamp_per_year']]/4), Settings[['Nyears']])
    a_i = rep(0.01, length(s_i))
  }else{
    NN_domain = RANN::nn2( data=Extrapolation_List$Data_Extrap[,c('Lon','Lat')], query=Data_Geostat[,c('Lon','Lat')], k=1)
    s_i = NN_domain$nn.idx[,1]
    t_i = Data_Geostat[,'Year'] - min(Data_Geostat[,'Year']) + 1
    v_i = as.numeric(factor(Data_Geostat[,'Vessel']))
    a_i = Data_Geostat[,'AreaSwept_km2']
  }

  # Generate locations, years, vessel for each sample i
  loc_s = Extrapolation_List$Data_Extrap[,c('E_km','N_km','Lat','Lon')]
  loc_i = loc_s[s_i,]

  # Generate intercepts
  exp_beta1_t = Settings[['beta1_mean']] + Settings[['beta1_slope']] * (1:max(t_i)-mean(1:max(t_i))/2)
  beta1_t = rnorm( max(t_i), mean=exp_beta1_t, sd=Settings[['beta1_sd']] )
  exp_beta2_t = Settings[['beta2_mean']] + Settings[['beta2_slope']] * (1:max(t_i)-mean(1:max(t_i))/2)
  beta2_t = rnorm( max(t_i), mean=exp_beta2_t, sd=Settings[['beta2_sd']] )

  # Simulate spatial and spatio-temporal components
  O1_s = RFsim(model=model_O1, x=loc_s[,'E_km'], y=loc_s[,'N_km'], standardize=standardize_fields)
  O2_s = RFsim(model=model_O2, x=loc_s[,'E_km'], y=loc_s[,'N_km'], standardize=standardize_fields)
  message( "Finished variation that is constant over time" )
  E1_st = E2_st = matrix(NA, nrow=nrow(loc_s), ncol=max(t_i) )
  for(t in 1:max(t_i)){
    E1_st[,t] = RFsim(model=model_E1, x=loc_s[,'E_km'], y=loc_s[,'N_km'], standardize=standardize_fields)
    E2_st[,t] = RFsim(model=model_E2, x=loc_s[,'E_km'], y=loc_s[,'N_km'], standardize=standardize_fields)
    message( "Finished variation for year ", t )
  }

  # Extract covariates
  X_sj = array( 0, dim=c(nrow(Extrapolation_List$Data_Extrap),3), dimnames=list(NULL,c('Depth_km','Depth_km2','Rock_dist_')) )
  for( colI in 1:ncol(X_sj)){
    if(colnames(X_sj)[colI] %in% colnames(Extrapolation_List$Data_Extrap)) X_sj[,colI] = as.matrix(Extrapolation_List$Data_Extrap[,c('Depth_km','Depth_km2','Rock_dist_')[colI]])
    X_sj = ifelse( is.na(X_sj), outer(rep(1,nrow(X_sj)),colMeans(X_sj,na.rm=TRUE)), X_sj)
  }

  # Simulate vessel effects
  Vessel1_vy = array( rnorm( n=max(v_i)*max(t_i), mean=0, sd=Settings[['SigmaVY1']]), dim=c(max(v_i),max(t_i)) )
  Vessel2_vy = array( rnorm( n=max(v_i)*max(t_i), mean=0, sd=Settings[['SigmaVY2']]), dim=c(max(v_i),max(t_i)) )
  Vessel1_v = array( rnorm( n=max(v_i), mean=0, sd=Settings[['SigmaV1']]), dim=c(max(v_i)) )
  Vessel2_v = array( rnorm( n=max(v_i), mean=0, sd=Settings[['SigmaV2']]), dim=c(max(v_i)) )

  # Calculate expected values
  P1_st = P2_st = matrix(NA, nrow=nrow(loc_s), ncol=max(t_i) )
  for(t in 1:max(t_i)){
    P1_st[,t] = beta1_t[t] + O1_s + E1_st[,t] + as.matrix(X_sj)%*%unlist(Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
    P2_st[,t] = beta2_t[t] + O2_s + E2_st[,t] + as.matrix(X_sj)%*%unlist(Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
  }

  # Calculate linear predictors
  P1_i = P1_st[cbind(s_i,t_i)] + Vessel1_vy[cbind(v_i,t_i)] + Vessel1_v[v_i]
  P2_i = P2_st[cbind(s_i,t_i)] + Vessel2_vy[cbind(v_i,t_i)] + Vessel2_v[v_i]

  # Calculate linked-predictors
  if( Settings[['ObsModel']][2]==0 ){
    R1_i = plogis( P1_i )
    R2_i = a_i * exp( P2_i )
  }
  if( Settings[['ObsModel']][2]==1 ){
    R1_i = 1 - exp( -1*a_i*exp(P1_i) )
    R2_i = a_i*exp(P1_i) / R1_i * exp(P2_i)
  }
  if( Settings[['ObsModel']][2]==2 ){
    R1_i = a_i * exp(P1_i)
    R2_i = exp(P2_i)
  }

  # Simulate catch and assemble data frame
  if( Settings[['ObsModel']][1]==2 ){
    b1_i = rbinom( n=length(R1_i), size=1, prob=R1_i )
    b2_i = rlnorm( n=length(R2_i), meanlog=log(R2_i)-Settings[['SigmaM']]^2/2, sdlog=Settings[['SigmaM']])
    b_i = b1_i * b2_i
  }
  if( Settings[['ObsModel']][1]==8 ){
    if( Settings[['ObsModel']][2]!=2 ) stop("Must use 'ObsModel=c(8,2)'")
    b_i = rep(NA,length(R1_i))
    for(i in 1:length(b_i)) b_i[i] = rPoisGam( n=1, shape=Settings[['SigmaM']], scale=R1_i[i], intensity=R2_i[i] )
  }
  if( Settings[['ObsModel']][1]==10 ){
    if( Settings[['ObsModel']][2]!=2 ) stop("Must use 'ObsModel=c(8,2)'")
    b_i = rep(NA,length(R1_i))                            # R1_i(i)*R2_i(i), R1_i(i), invlogit(SigmaM(c_i(i),0))+1.0
    for(i in 1:length(b_i)) b_i[i] = tweedie::rtweedie(n=1, mu=R1_i[i]*R2_i[i], phi=R1_i[i], power=plogis(Settings[['SigmaM']])+1.0)
  }
  Data_Geostat = cbind( "Catch_KG"=b_i, "Year"=t_i, "Vessel"=v_i, "AreaSwept_km2"=a_i, "Lat"=loc_i[,'Lat'], "Lon"=loc_i[,'Lon'] )

  # Calculate true spatio-temporal biomass and annual abundance
  if( Settings[['ObsModel']][2]==0 ){
    B_st = plogis(P1_st) * exp(P2_st) * outer(Extrapolation_List$Area_km2_x,rep(1,max(t_i)))
  }
  if( Settings[['ObsModel']][2] %in% c(1,2) ){
    B_st = exp(P1_st) * exp(P2_st) * outer(Extrapolation_List$Area_km2_x,rep(1,max(t_i)))
  }

  # Calculate true center-of-gravity (COG)
  COG_tm = array(NA, dim=c(max(t_i),4), dimnames=list(NULL,c("Lat","Lon","E_km","N_km")))
  for( tI in 1:nrow(COG_tm)){
  for( mI in 1:ncol(COG_tm)){
    COG_tm[tI,mI] = weighted.mean( x=Extrapolation_List$Data_Extrap[,colnames(COG_tm)[mI]], w=B_st[,tI] )
  }}

  # Calculate stratified biomass
  B_tl = array(NA, dim=c(max(t_i),ncol(Extrapolation_List$a_el)), dimnames=list(NULL,colnames(Extrapolation_List$a_el)))
  for( l in 1:ncol(B_tl) ){
    B_tl[,l] = colSums( B_st * outer(Extrapolation_List$a_el[,l]/Extrapolation_List$Area_km2_x,rep(1,max(t_i))), na.rm=TRUE )
  }

  # plot data
  if(MakePlot==TRUE){
    f = function(Num) ((Num)-min((Num),na.rm=TRUE))/diff(range((Num),na.rm=TRUE))
    Col = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red"))
    Xlim = c(-126,-117); Ylim = c(32,49)
    MapSizeRatio = c( "Height(in)"=4, "Width(in)"=2 )
    for(RespI in 1:3){
      Mat = matrix(NA, ncol=max(t_i), nrow=nrow(Extrapolation_List$Data_Extrap))
      for(t in 1:max(t_i)){
        if(RespI==1) Mat[,t] = plogis(P1_st[,t])
        if(RespI==2) Mat[,t] = exp(P2_st[,t])
        if(RespI==3) Mat[,t] = plogis(P1_st[,t]) * exp(P2_st[,t])
      }
      png(file=paste(DateFile,"True_",switch(RespI, "Pres","Pos","Dens"),".png",sep=""), width=5*MapSizeRatio['Width(in)'], height=2*MapSizeRatio['Height(in)'], res=200, units='in')
        par(mfrow=c(2,5), mar=c(2,2,0,0))
        for(t in 1:max(t_i)){
          maps::map("worldHires", ylim=Ylim, xlim=Xlim, col="grey90",fill=T, main="", mar=c(0,0,2.5,0),interior=TRUE)
          points(x=Extrapolation_List$Data_Extrap[,'Lon'], y=Extrapolation_List$Data_Extrap[,'Lat'], col=Col(n=50)[ceiling(f(Mat)[,t]*49)+1], cex=0.01, pch=20)
        }
      dev.off()
      # Legend
      png(file=paste0(DateFile,"True_",switch(RespI, "Pres","Pos","Dens","Pos_Rescaled","Dens_Rescaled"),"_Legend.png",sep=""), width=1, height=2*MapSizeRatio['Height(in)'], res=200, units='in')
        SpatialDeltaGLMM:::Heatmap_Legend( colvec=Col(n=50), heatrange=range(Mat), textmargin=switch(RespI, "Encounter probability","log(Positive catch rate)",expression(paste("log Density, log(kg. / ",km^2,")",sep="")),NULL,NULL) )
      dev.off()
    }
  }
  time_for_simulation = Sys.time()-start_time
  message( "Total time: ", time_for_simulation )

  # Return stuff
  Return = list( "Data_Geostat"=data.frame(Data_Geostat), "B_tl"=B_tl, "B_st"=B_st, "COG_tm"=COG_tm, "beta1_t"=beta1_t, "beta2_t"=beta2_t, "O1_s"=O1_s, "O2_s"=O2_s, "E1_st"=E1_st,
    "Vessel1_vy"=Vessel1_vy, "Vessel2_vy"=Vessel2_vy, "Vessel1_v"=Vessel1_v, "Vessel2_v"=Vessel2_v, "E2_st"=E2_st, "P1_st"=P1_st,
    "P2_st"=P2_st, "s_i"=s_i, "t_i"=t_i, "P1_i"=P1_i, "P2_i"=P2_i, "R1_i"=R1_i, "R2_i"=R2_i, "time_for_simulation"=time_for_simulation, "Sim_Settings"=Settings )
  if( exists("b1_i")) Return = c(Return, list("b1_i"=b1_i, "b2_i"=b2_i))
  return( Return )
}
