
#' @export
Geostat_Sim <-
function(Sim_Settings, Extrapolation_List, MakePlot=TRUE, DateFile=paste0(getwd(),"/") ){
  # Terminology
  # O: Spatial component; E: spatiotemporal component
  # O1: presence-absence; O2: positive catch rate
  # P1 : linear predictor of presence/absence in link-space
  # R1: predictor of presence/absence in natural-space (transformed out of link-space)
  # v_i: vessel for sample i
  # t_i: year for sample i

  # Attach stuff
  start_time = Sys.time()
  library( maps )
  library( mapdata )
  on.exit( detach(package:mapdata), add=TRUE )
  on.exit( detach(package:maps), add=TRUE )

  # Initialize random-field objects
  model_O1 = RandomFields::RMgauss(var=Sim_Settings[['SigmaO1']]^2, scale=Sim_Settings[['Range1']])
  model_E1 = RandomFields::RMgauss(var=Sim_Settings[['SigmaE1']]^2, scale=Sim_Settings[['Range1']])
  model_O2 = RandomFields::RMgauss(var=Sim_Settings[['SigmaO2']]^2, scale=Sim_Settings[['Range2']])
  model_E2 = RandomFields::RMgauss(var=Sim_Settings[['SigmaE2']]^2, scale=Sim_Settings[['Range2']])

  # Generate locations, years, vessel for each sample i
  s_i = sample(1:nrow(Extrapolation_List$Data_Extrap), size=Sim_Settings[['Nyears']]*Sim_Settings[['Nsamp_per_year']]) #, nrow=Sim_Settings[['Nsamp_per_year']], ncol=Sim_Settings[['Nyears']])
  loc_s = Extrapolation_List$Data_Extrap[,c('E_km','N_km','Lat','Lon')]
  loc_i = loc_s[s_i,]
  t_i = rep( 1:Sim_Settings[['Nyears']], each=Sim_Settings[['Nsamp_per_year']])
  v_i = rep(rep( 1:4, each=Sim_Settings[['Nsamp_per_year']]/4), Sim_Settings[['Nyears']])
  a_i = rep(0.01, length(s_i))

  # Generate intercepts
  exp_beta1_t = Sim_Settings[['beta1_mean']] + Sim_Settings[['beta1_slope']] * (1:Sim_Settings[['Nyears']]-mean(1:Sim_Settings[['Nyears']])/2)
  beta1_t = rnorm(Sim_Settings[['Nyears']], mean=exp_beta1_t, sd=Sim_Settings[['beta1_sd']] )
  exp_beta2_t = Sim_Settings[['beta2_mean']] + Sim_Settings[['beta2_slope']] * (1:Sim_Settings[['Nyears']]-mean(1:Sim_Settings[['Nyears']])/2)
  beta2_t = rnorm(Sim_Settings[['Nyears']], mean=exp_beta2_t, sd=Sim_Settings[['beta2_sd']] )

  # Simulate spatial and spatio-temporal components
  O1_s = RandomFields::RFsimulate(model=model_O1, x=loc_s[,'E_km'], y=loc_s[,'N_km'])@data[,1]
  O2_s = RandomFields::RFsimulate(model=model_O2, x=loc_s[,'E_km'], y=loc_s[,'N_km'])@data[,1]
  message( "Finished variation that is constant over time" )
  E1_st = E2_st = matrix(NA, nrow=nrow(loc_s), ncol=Sim_Settings[['Nyears']] )
  for(t in 1:Sim_Settings[['Nyears']]){
    E1_st[,t] = RandomFields::RFsimulate(model=model_E1, x=loc_s[,'E_km'], y=loc_s[,'N_km'])@data[,1]
    E2_st[,t] = RandomFields::RFsimulate(model=model_E2, x=loc_s[,'E_km'], y=loc_s[,'N_km'])@data[,1]
    message( "Finished variation for year ", t )
  }

  # Extract covariates
  X_sj = as.matrix(Extrapolation_List$Data_Extrap[,c('Depth_km','Depth_km2','Rock_dist_')])
  X_sj = ifelse( is.na(X_sj), outer(rep(1,nrow(X_sj)),colMeans(X_sj,na.rm=TRUE)), X_sj)

  # Simulate vessel effects
  Vessel_vyc = array( rnorm( n=4*Sim_Settings[['Nyears']]*2, mean=0, sd=Sim_Settings[['SigmaVY1']]), dim=c(4,Sim_Settings[['Nyears']],2))

  # Calculate expected values
  P1_st = P2_st = matrix(NA, nrow=nrow(loc_s), ncol=Sim_Settings[['Nyears']] )
  for(t in 1:Sim_Settings[['Nyears']]){
    P1_st[,t] = beta1_t[t] + O1_s + E1_st[,t] + as.matrix(X_sj)%*%unlist(Sim_Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
    P2_st[,t] = beta2_t[t] + O2_s + E2_st[,t] + as.matrix(X_sj)%*%unlist(Sim_Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
  }
  R1_i = plogis( P1_st[cbind(s_i,t_i)] + Vessel_vyc[v_i[which(t_i==t)],t,1] )
  R2_i = a_i * exp( P2_st[cbind(s_i,t_i)] + Vessel_vyc[v_i[which(t_i==t)],t,2] )

  # Simulate catch and assemble data frame
  b1_i = rbinom( n=length(R1_i), size=1, prob=R1_i )
  b2_i = rlnorm( n=length(R2_i), meanlog=log(R2_i)-Sim_Settings[['SigmaM']]^2/2, sdlog=Sim_Settings[['SigmaM']])
  b_i = b1_i * b2_i
  Data_Geostat = cbind( "Catch_KG"=b_i, "Year"=t_i, "Vessel"=v_i, "AreaSwept_km2"=a_i, "Lat"=loc_i[,'Lat'], "Lon"=loc_i[,'Lon'] )

  # Calculate true spatio-temporal biomass and annual abundance
  B_st = plogis(P1_st) * exp(P2_st) * outer(Extrapolation_List$Area_km2_x,rep(1,Sim_Settings[['Nyears']]))
  B_t = colSums( B_st )

  # plot data
  if(MakePlot==TRUE){
    f = function(Num) ((Num)-min((Num),na.rm=TRUE))/diff(range((Num),na.rm=TRUE))
    Col = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red"))
    Xlim = c(-126,-117); Ylim = c(32,49)
    MapSizeRatio = c( "Height(in)"=4, "Width(in)"=2 )
    for(RespI in 1:3){
      Mat = matrix(NA, ncol=Sim_Settings[['Nyears']], nrow=nrow(Extrapolation_List$Data_Extrap))
      for(t in 1:Sim_Settings[['Nyears']]){
        if(RespI==1) Mat[,t] = plogis(P1_st[,t])
        if(RespI==2) Mat[,t] = exp(P2_st[,t])
        if(RespI==3) Mat[,t] = plogis(P1_st[,t]) * exp(P2_st[,t])
      }
      png(file=paste(DateFile,"True_",switch(RespI, "Pres","Pos","Dens"),".png",sep=""), width=5*MapSizeRatio['Width(in)'], height=2*MapSizeRatio['Height(in)'], res=200, units='in')
        par(mfrow=c(2,5), mar=c(2,2,0,0))
        for(t in 1:Sim_Settings[['Nyears']]){
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
  Return = list( "Data_Geostat"=Data_Geostat, "B_t"=B_t, "beta1_t"=beta1_t, "beta2_t"=beta2_t, "O1_s"=O1_s, "O2_s"=O2_s, "E1_st"=E1_st,
    "E2_st"=E2_st, "P1_st"=P1_st, "P2_st"=P2_st, "s_i"=s_i, "t_i"=t_i, "b1_i"=b1_i, "b2_i"=b2_i, "time_for_simulation"=time_for_simulation )
  return( Return )
}
