
#' @export
Geostat_Sim <-
function(Sim_Settings, Extrapolation_List, MakePlot=TRUE){
  # Terminology
  # O: Spatial component; E: spatiotemporal component
  # O1: presence-absence; O2: positive catch rate
  # P1 : linear predictor of presence/absence in link-space
  # R1: predictor of presence/absence in natural-space (transformed out of link-space)
  # v_i: vessel for sample i
  # t_i: year for sample i

  # Attach stuff
  attach( Extrapolation_List )
  on.exit( detach(Extrapolation_List) )

  # Initialize GM models
  model_O1 = RandomFields::RMgauss(var=Sim_Settings[['SigmaO1']]^2, scale=Sim_Settings[['Range1']])
  model_E1 = RandomFields::RMgauss(var=Sim_Settings[['SigmaE1']]^2, scale=Sim_Settings[['Range1']])
  model_O2 = RandomFields::RMgauss(var=Sim_Settings[['SigmaO2']]^2, scale=Sim_Settings[['Range2']])
  model_E2 = RandomFields::RMgauss(var=Sim_Settings[['SigmaE2']]^2, scale=Sim_Settings[['Range2']])

  # Generate locations, years, vessel for each sample i
  s_i = sample(1:nrow(Data_Extrap), size=Sim_Settings[['Nyears']]*Sim_Settings[['Nsamp_per_year']]) #, nrow=Sim_Settings[['Nsamp_per_year']], ncol=Sim_Settings[['Nyears']])
  loc_i = Data_Extrap[s_i,c('E_km','N_km','Lat','Lon')]
  t_i = rep( 1:Sim_Settings[['Nyears']], each=Sim_Settings[['Nsamp_per_year']])
  v_i = rep(rep( 1:4, each=Sim_Settings[['Nsamp_per_year']]/4), Sim_Settings[['Nyears']])

  # Simulate random components
  O1_i = RandomFields::RFsimulate(model=model_O1, x=loc_i[,'E_km'], y=loc_i[,'N_km'])@data[,1] #), nrow=Sim_Settings[['Nsamp_per_year']], ncol=Sim_Settings[['Nyears']])
  O2_i = RandomFields::RFsimulate(model=model_O2, x=loc_i[,'E_km'], y=loc_i[,'N_km'])@data[,1] #), nrow=Sim_Settings[['Nsamp_per_year']], ncol=Sim_Settings[['Nyears']])
  E1_i = E2_i = rep(NA, Sim_Settings[['Nsamp_per_year']]*Sim_Settings[['Nyears']] )
  for(t in 1:Sim_Settings[['Nyears']]){
    E1_i[which(t_i==t)] = RandomFields::RFsimulate(model=model_E1, x=loc_i[which(t_i==t),'E_km'], y=loc_i[which(t_i==t),'N_km'])@data[,1]
    E2_i[which(t_i==t)] = RandomFields::RFsimulate(model=model_E2, x=loc_i[which(t_i==t),'E_km'], y=loc_i[which(t_i==t),'N_km'])@data[,1]
  }
  X_ij = as.matrix(Data_Extrap[s_i,c('Depth_km','Depth_km2','Rock_dist_')])
  X_ij = ifelse( is.na(X_ij), outer(rep(1,nrow(X_ij)),colMeans(X_ij,na.rm=TRUE)), X_ij)
  Vessel_vyc = array( rnorm( n=4*Sim_Settings[['Nyears']]*2, mean=0, sd=Sim_Settings[['SigmaVY1']]), dim=c(4,Sim_Settings[['Nyears']],2))

  # Calculate expected values, and simulate
  P1_i = O1_i + E1_i + as.matrix(X_ij)%*%unlist(Sim_Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
  R1_i = plogis( P1_i + Vessel_vyc[v_i[which(t_i==t)],t,1] )
  P2_i = O2_i + E2_i + as.matrix(X_ij)%*%unlist(Sim_Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
  R2_i = exp( P2_i + Vessel_vyc[v_i[which(t_i==t)],t,2] )
  CPUE_i = rlnorm( n=length(R2_i), meanlog=log(R2_i), sdlog=Sim_Settings[['SigmaM']]) * rbinom( n=length(R1_i), size=1, prob=R1_i )
  Data_Geostat = cbind( "Catch_KG"=CPUE_i, "Year"=t_i, "Vessel"=v_i, "AreaSwept_km2"=1/1e2, "Lat"=loc_i[,'Lat'], "Lon"=loc_i[,'Lon'] )

  # plot data
  if(MakePlot==TRUE){
    f = function(Num) ((Num)-min((Num),na.rm=TRUE))/diff(range((Num),na.rm=TRUE))
    Col = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red"))
    Xlim = c(-126,-117); Ylim = c(32,49)
    MapSizeRatio = c( "Height(in)"=4, "Width(in)"=2 )
    for(RespI in 1:5){
      Mat = matrix(NA, ncol=Sim_Settings[['Nyears']], nrow=nrow(Data_Extrap))
      for(t in 1:Sim_Settings[['Nyears']]){
        NN_Extrap = nn2( data=loc_i[which(t_i==t),c('E_km','N_km')], query=Data_Extrap[,c('E_km','N_km')], k=1 )
        if(RespI==1) Mat[,t] = (R1_i[which(t_i==t)])[NN_Extrap$nn.idx]
        if(RespI==2) Mat[,t] = (R2_i[which(t_i==t)])[NN_Extrap$nn.idx]
        if(RespI==3) Mat[,t] = (R1_i[which(t_i==t)]*R2_i[which(t_i==t)])[NN_Extrap$nn.idx]
        if(RespI==4) Mat[,t] = (log(R2_i[which(t_i==t)]+quantile(R2_i[which(t_i==t)],0.25)))[NN_Extrap$nn.idx]
        if(RespI==5) Mat[,t] = (log(R1_i[which(t_i==t)]*R2_i[which(t_i==t)]+quantile(R1_i[which(t_i==t)]*R2_i[which(t_i==t)],0.25)))[NN_Extrap$nn.idx]
        if(RespI==3) True_Index = colSums( Mat )
      }
      png(file=paste(DateFile,"True_",switch(RespI, "Pres","Pos","Dens","Pos_Rescaled","Dens_Rescaled"),".png",sep=""), width=5*MapSizeRatio['Width(in)'], height=2*MapSizeRatio['Height(in)'], res=200, units='in')
        par(mfrow=c(2,5), mar=c(2,2,0,0))
        for(t in 1:Sim_Settings[['Nyears']]){
          maps::map("worldHires", ylim=Ylim, xlim=Xlim, col="grey90",fill=T, main="", mar=c(0,0,2.5,0),interior=TRUE)
          points(x=Data_Extrap[,'Lon'], y=Data_Extrap[,'Lat'], col=Col(n=10)[ceiling(f(Mat)[,t]*9)+1], cex=0.01)
          #points( x=Data_Geostat[,'Lon'], y=Data_Geostat[,'Lat'], col=Col(n=10)[ceiling(f(Mat)[,t]*9)+1], cex=0.05)
        }
      dev.off()
      # Legend
      png(file=paste0(DateFile,"True_",switch(RespI, "Pres","Pos","Dens","Pos_Rescaled","Dens_Rescaled"),"_Legend.png",sep=""), width=1, height=2*MapSizeRatio['Height(in)'], res=200, units='in')
        SpatialDeltaGLMM:::Heatmap_Legend( colvec=Col(n=50), heatrange=range(Mat), margintext=switch(RespI, "Encounter probability","log(Positive catch rate)",expression(paste("log Density, log(kg. / ",km^2,")",sep="")),NULL,NULL) )
      dev.off()
    }
  }
  Return = list( "Data_Geostat"=Data_Geostat, "True_Index"=True_Index )
  return( Return )
}
