
# Plot predicted surface (density, encounter probability, etc.)
PlotMap_Fn = function(MappingDetails, Report, MapSizeRatio, Xlim, Ylim, FileName, Year_Set){
  D_it = Report$D_xt[NN_Extrap$nn.idx,]
  R1_it = Report$R1_xt[NN_Extrap$nn.idx,]
  R2_it = Report$R2_xt[NN_Extrap$nn.idx,]
  f = function(Num) ((Num)-min((Num),na.rm=TRUE))/diff(range((Num),na.rm=TRUE))
  Col = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red"))
  for(RespI in 1:5){
    if(RespI==1) Mat = R1_it
    if(RespI==2){
      Mat = log(R2_it+quantile(R2_it,0.01))
      Mat = ifelse(Mat<(-5),-5,Mat)
    }
    if(RespI==3){
      Mat = log(D_it+quantile(D_it,0.01))
      Mat = ifelse(Mat<(-5),-5,Mat)
    }
    if(RespI==4) Mat = log(R2_it+quantile(R2_it,0.25))   # 
    if(RespI==5) Mat = log(D_it+quantile(D_it,0.25))  # 
    # Plot 
    png(file=paste0(FileName,switch(RespI, "Pres","Pos","Dens","Pos_Rescaled","Dens_Rescaled"),".png"), width=5*MapSizeRatio['Width(in)'], height=2*MapSizeRatio['Height(in)'], res=400, units='in')
      par(mfrow=c(2,6), oma=c(2,2,0,0))
      for(t in 1:length(Year_Set)){
        Which = which(Data_Extrap[,'Include']==TRUE)
        map(MappingDetails[[1]], MappingDetails[[2]], ylim=Ylim, xlim=Xlim, col="grey90", fill=TRUE, main="", mar=c(0,0,2,0),interior=TRUE)#, main=Year_Set[t])
        points(x=Data_Extrap[Which,'Lon'], y=Data_Extrap[Which,'Lat'], col=Col(n=50)[ceiling(f(Mat[Which,])[,t]*49)+1], cex=0.01)
        title( Year_Set[t], line=0.1, cex.main=1.5 )
        box()
      }
      mtext(side=1, outer=TRUE, "Longitude", cex=1.75)
      mtext(side=2, outer=TRUE, "Latitude", cex=1.75)
    dev.off()
    # Legend
    png(file=paste0(FileName,switch(RespI, "Pres","Pos","Dens","Pos_Rescaled","Dens_Rescaled"),"_Legend.png",sep=""), width=1, height=2*MapSizeRatio['Height(in)'], res=200, units='in')
      Heatmap_Legend( colvec=Col(n=50), heatrange=range(Mat), margintext=switch(RespI, "Encounter probability","log(Positive catch rate)",expression(paste("log Density, log(kg. / ",km^2,")",sep="")),NULL,NULL) )
    dev.off()
  }
}


# Plot effect of covariates
PlotCov_Fn = function(Report, NN_Extrap, X_xj, FileName, ControlList=list("Width"=5*3, "Height"=2*3, "Res"=200, "Units"='in')){ 
  # Extract outputs
  D_it = Report$D_xt[NN_Extrap$nn.idx,]
  R1_it = Report$R1_xt[NN_Extrap$nn.idx,]
  R2_it = Report$R2_xt[NN_Extrap$nn.idx,]
  for(CovI in 1:ncol(X_xj)){
    png(file=paste0(FileName,colnames(X_xj)[CovI],".png"), width=ControlList$Width, height=ControlList$Height, res=ControlList$Res, units=ControlList$Units)
      par(mfrow=c(2,6), oma=c(0,0,2,0), mar=c(2,2,0,0), mgp=c(1.5,0.5,0), tck=-0.02)
      for(t in 1:length(Year_Set)){
        #plot(x=Data_Extrap[,'Depth_km'], y=D_it[,t], type="p")
        plot(x=X_xj[NN_Extrap$nn.idx,CovI], y=R2_it[,t], type="p", col="red", ylim=c(0,max(R2_it)))
        points(x=X_xj[NN_Extrap$nn.idx,CovI], y=R1_it[,t]*max(R2_it), type="p", col="blue")
        points(x=X_xj[NN_Extrap$nn.idx,CovI], y=D_it[,t], type="p", col="black")
      }
      mtext( side=3, outer=TRUE, colnames(X_xj)[CovI] )
    dev.off()
  }
}

# Plot time series descriptive plots
Timeseries_Fn = function(Report, FileName, ControlList=list("Width"=4*3, "Height"=2*3, "Res"=200, "Units"='in')){
  png(file=FileName, width=ControlList$Width, height=ControlList$Height, res=ControlList$Res, units=ControlList$Units)
    par(mfrow=c(2,4), oma=c(0,0,0,0), mar=c(3,3,2,0), mgp=c(1.5,0.5,0), tck=-0.02)    
    # Entropy
    EntropyFn = function(Vec){ sum(Vec * log(Vec+1e-250)/log(length(Vec)) ) }
    Entropy_t = apply( Report$D_xt, MARGIN=2, FUN=EntropyFn )
    plot( x=Year_Set, y=Entropy_t, type="l", main="Entropy")
    # Variance
    Var_t = apply( Report$D_xt, MARGIN=2, FUN=var )
    plot( x=Year_Set, y=Var_t, type="l", main="Variance", ylim=c(0,max(Var_t)) )
    # CV
    CV_t = apply( Report$D_xt, MARGIN=2, FUN=function(Vec){ sd(Vec)/mean(Vec) } )
    plot( x=Year_Set, y=CV_t, type="l", main="CV", ylim=c(0,max(CV_t)) )
    # Occupancy probability
    Occup_t = colMeans( R1_it )
    plot( x=Year_Set, y=Occup_t, type="l", main="Occup_t", ylim=c(0,1) )    
    # Density
    CondDens_t = colMeans( R2_it )
    plot( x=Year_Set, y=CondDens_t, type="l", main="CondDens_t", ylim=c(0,max(CondDens_t)) )    
    # Density
    Index_t = colMeans( D_it )
    plot( x=Year_Set, y=Index_t, type="l", main="Index_t", ylim=c(0,max(Index_t)) )    
    # correlation between occupancy and abundance
    Cor_t = sapply(1:length(Year_Set), FUN=function(Num){cor(R1_it[,Num],R2_it[,Num],method="spearman")}) 
    plot( x=Year_Set, y=Cor_t, type="l", main="Cor_t", ylim=c(-1,1) ); abline(h=0, lty="dotted")      
  dev.off()
}


# Build Map (for turning off parameters in TMB)
Make_Map = function( VesselConfig, TmbData, FieldConfig, CovConfig, CovConception, ObsModel, Aniso){
  Map = list()
  if(VesselConfig['Vessel']==0){
    Map[["nu1_v"]] = factor(rep(NA,TmbData$n_v)) 
    Map[["nu2_v"]] = factor(rep(NA,TmbData$n_v))
    Map[["logsigmaV1"]] = factor(NA) 
    Map[["logsigmaV2"]] = factor(NA) 
  }
  if(VesselConfig['VesselYear']==0){
    Map[["nu1_vt"]] = factor(matrix(NA,nrow=TmbData$n_v,ncol=TmbData$n_t))  
    Map[["nu2_vt"]] = factor(matrix(NA,nrow=TmbData$n_v,ncol=TmbData$n_t)) 
    Map[["logsigmaVT1"]] = factor(NA) 
    Map[["logsigmaVT2"]] = factor(NA) 
  }
  if(FieldConfig['Omega1']==0){
    Map[["Omegainput1_s"]] = factor(rep(NA,TmbData$n_s))
    Map[["logetaO1"]] = factor(NA) 
  }
  if(FieldConfig['Epsilon1']==0){
    Map[["Epsiloninput1_st"]] = factor(matrix(NA,nrow=TmbData$n_s,ncol=TmbData$n_t))
    Map[["logetaE1"]] = factor(NA) 
  }
  if(FieldConfig['Omega1']==0 & FieldConfig['Epsilon1']==0) Map[["logkappa1"]] = factor(NA)
  if(FieldConfig['Omega2']==0){
    Map[["Omegainput2_s"]] = factor(rep(NA,TmbData$n_s))
    Map[["logetaO2"]] = factor(NA) 
  }
  if(FieldConfig['Epsilon2']==0){
    Map[["Epsiloninput2_st"]] = factor(matrix(NA,nrow=TmbData$n_s,ncol=TmbData$n_t))
    Map[["logetaE2"]] = factor(NA) 
  }
  if(FieldConfig['Omega2']==0 & FieldConfig['Epsilon2']==0) Map[["logkappa2"]] = factor(NA)
  if( sum(CovConfig)==0 & CovConception==FALSE ){
    Map[["gamma1_j"]] = factor(NA)
    Map[["gamma2_j"]] = factor(NA)
  }
  if(ObsModel%in%c(0,1,2)){
    Map[["logSigmaM"]] = factor( c(1,NA,NA) )
  }
  if(ObsModel%in%c(4,5)){
    Map[["logSigmaM"]] = factor( c(1,NA,2) )
  }
  if(Aniso==0 | all(FieldConfig==0)) Map[['ln_H_input']] = factor( rep(NA,2) )
  return(Map)
}

# Calculate stuff for anistropic mesh
Calc_Anisotropic_Mesh = function(loc_x){
  # Create the SPDE mesh
  mesh = inla.mesh.create( loc_x, plot.delay=NULL, refine=FALSE)
  spde = inla.spde2.matern(mesh, alpha=2)

  # Pre-processing in R for anisotropy
  Dset = 1:2
  # Triangle info
  TV = mesh$graph$tv       # Triangle to vertex indexing
  V0 = mesh$loc[TV[,1],Dset]   # V = vertices for each triangle
  V1 = mesh$loc[TV[,2],Dset]
  V2 = mesh$loc[TV[,3],Dset]
  E0 = V2 - V1                      # E = edge for each triangle
  E1 = V0 - V2
  E2 = V1 - V0
  
  # Calculate Areas
  TmpFn = function(Vec1,Vec2) abs(det( rbind(Vec1,Vec2) ))
  Tri_Area = rep(NA, nrow(E0))
  for(i in 1:length(Tri_Area)) Tri_Area[i] = TmpFn( E0[i,],E1[i,] )/2   # T = area of each triangle

  # Return stuff
  Return = list("Tri_Area"=Tri_Area, "TV"=TV, "E0"=E0, "E1"=E1, "E2"=E2, "mesh"=mesh, "spde"=spde)
  return(Return)
}

# Apply K-means algorithm to identify location for knots
Calc_Kmeans = function(n_x, Kmeans_Config, Data_Geostat, Data_Extrap){
  old.options <- options()
  options( "warn" = -1 ) 
  on.exit( options(old.options) )   
  if( paste0("Kmeans-",n_x,".RData") %in% list.files(getwd()) ){
    load( file=paste(DateFile,"Kmeans.RData",sep=""))
  }else{
    Kmeans = list( "tot.withinss"=Inf )
    for(i in 1:Kmeans_Config[["nstart"]]){       
      if(Kmeans_Config[["Locs"]]=="Samples"){
        Tmp = kmeans( x=Data_Geostat[,c('E_km','N_km')], centers=n_x, iter.max=Kmeans_Config[["iter.max"]], nstart=1, trace=0)
      }
      if(Kmeans_Config[["Locs"]]=="Domain"){
        Tmp = kmeans( x=Data_Extrap[,c('E_km','N_km')], centers=n_x, iter.max=Kmeans_Config[["iter.max"]], nstart=1, trace=0) # K$tot.withinss
      }
      print( paste0('Num=',i,' Current_Best=',round(Kmeans$tot.withinss,1),' New=',round(Tmp$tot.withinss,1)) )#,' Time=',round(Time,4)) )
      if( Tmp$tot.withinss < Kmeans$tot.withinss ){
        Kmeans = Tmp
      }
    }
  }
  return( Kmeans )
}

# Generate heatmap legend
Heatmap_Legend = function( colvec, heatrange, margintext=NULL ){
  par( xaxs="i", yaxs="i", mar=c(1,0,1,2+ifelse(is.null(margintext),0,1.5)), mgp=c(1.5,0.25,0), tck=-0.02 )
  N = length(colvec)
  Y = seq(heatrange[1], heatrange[2], length=N+1)
  plot( 1, type="n", xlim=c(0,1), ylim=heatrange, xlab="", ylab="", main="", xaxt="n", yaxt="n", cex.main=1.5)
  for( i in 1:N) polygon( x=c(0,1,1,0), y=Y[c(i,i,i+1,i+1)], col=colvec[i], border=NA)
  axis(side=4, at=pretty(heatrange) )
  if(!is.null(margintext)) mtext(side=4, text=margintext, line=2, cex=1.5)
  #mtext(side=1, outer=TRUE, line=1, "Legend")
}

# Non-zeroes: Simulate predictions, and calculate log-likelihood of the data for use in WAIC
#FileName_PP = paste(DateFile,"Posterior_Predictive.jpg",sep="")
#FileName_QQ = paste(DateFile,"Q-Q_plot.jpg",sep="")
QQ_Fn = function(TmbData, Report, FileName_PP=NULL, FileName_Phist=NULL, FileName_QQ=NULL, FileName_Qhist=NULL){
  attach(TmbData)
  on.exit( detach(TmbData) )
  attach(Report)
  on.exit( detach(Report) )
  pow = function(a,b) a^b
  Which = which(b_i>0)
  Q = rep(NA, length(Which) ) # vector to track quantiles for each observation
  y = array(NA, dim=c(length(Which),1000))
  
  # Make plot while calculating posterior predictives
    if( !is.null(FileName_PP) ) jpeg(FileName_PP,width=10,height=3,res=200,units="in")
      par(mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
        plot( b_i[Which], ylab="", xlab="",log="y", main="", col="blue")
        # mean(u.nz[,2])
        for(ObsI in 1:length(Which)){
          Pred = (R2_i[Which[ObsI]]*a_i[Which[ObsI]])
          if(TmbData$ObsModel==1){     
            y[ObsI,] = rlnorm(n=ncol(y), meanlog=log(Pred)-pow(SigmaM[1],2)/2, sdlog=SigmaM[1])   # Plotting in log-space
          }
          if(TmbData$ObsModel==2){     
            b = pow(SigmaM[1],2) * Pred;    
            y[ObsI,] = rgamma(n=ncol(y), shape=1/pow(SigmaM[1],2), scale=b)
          }
          if(TmbData$ObsModel==11){     
            ECE = rbinom(n=1000, size=1, prob=1-SigmaM[2])
            y[ObsI,] = rlnorm(n=ncol(y), meanlog=log(Pred)-pow(SigmaM[1],2)/2, sdlog=SigmaM[1])*(1-ECE) + rlnorm(n=ncol(y), meanlog=log(Pred)-pow(SigmaM[1],2)/2+log(1+SigmaM[3]), sdlog=SigmaM[1])*ECE
          }
          if(TmbData$ObsModel==12){     
            b = pow(SigmaM[1],2) * Pred;    
            b2 = pow(SigmaM[1],2) * Pred * (1+SigmaM[3]);    
            ECE = rbinom(n=ncol(y), size=1, prob=1-SigmaM[2])
            y[ObsI,] = rgamma(n=ncol(y), shape=1/pow(SigmaM[1],2), scale=b)*(1-ECE) + rgamma(n=ncol(y), shape=1/pow(SigmaM[1],2), scale=b2)*ECE
          }
          Q[ObsI] = mean(y[ObsI,]>b_i[Which[ObsI]])
          Quantiles = quantile(y[ObsI,],prob=c(0.025,0.25,0.75,0.975))
          lines(x=c(ObsI,ObsI), y=Quantiles[2:3], lwd=2)
          lines(x=c(ObsI,ObsI), y=Quantiles[c(1,4)], lwd=1,lty="dotted")
          if(b_i[Which[ObsI]]>max(Quantiles) | b_i[Which[ObsI]]<min(Quantiles)){
            points(x=ObsI,y=b_i[Which[ObsI]],pch=4,col="red",cex=2)
          }
        }
    if( !is.null(FileName_PP) ) dev.off()
  
  # Q-Q plot
  if( !is.null(FileName_QQ) ) jpeg(FileName_QQ,width=4,height=4,res=200,units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    Qtemp = na.omit(Q)
    Order = order(Qtemp)
    plot(x=seq(0,1,length=length(Order)), y=Qtemp[Order], main="Q-Q plot", xlab="Uniform", ylab="Empirical")
    abline(a=0,b=1)
  if( !is.null(FileName_QQ) ) dev.off()
  
  # Aggregate predictive distribution
  if( !is.null(FileName_Phist) ) jpeg(FileName_Phist,width=4,height=4,res=200,units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    hist( log(y), main="Aggregate predictive dist.", xlab="log(Obs)", ylab="Density")
  if( !is.null(FileName_Phist) ) dev.off()
  
  # Quantile histogram
  if( !is.null(FileName_Qhist) ) jpeg(FileName_Qhist,width=4,height=4,res=200,units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    hist(na.omit(Q), main="Quantile_histogram", xlab="Quantile", ylab="Number")
  if( !is.null(FileName_Qhist) ) dev.off()
  
  # Return stuff
  Return = list( "Q"=Q )
  return( Return )
}  

# Vessel effect plotting
Vessel_Fn = function( TmbData, Sdreport, FileName_VYplot=NULL ){
  attach(TmbData)
  on.exit( detach(TmbData) )
  Summary = summary(Sdreport)

  nu_vt = array(NA, dim=c(n_v,n_t,2,2))
  for(vI in 1:n_v){
  for(tI in 1:n_t){
    Num = (vI-1)*n_t + tI
    nu_vt[vI,tI,1,] = Summary[which(rownames(Summary)=="nu1_vt")[Num],]
    nu_vt[vI,tI,2,] = Summary[which(rownames(Summary)=="nu2_vt")[Num],]    
  }}
  
  if( !is.null(FileName_VYplot) ) jpeg(FileName_VYplot, width=1.5*n_t,height=5,res=200,units="in")
    par(mfrow=c(2,n_t), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02, oma=c(0,3,0,0))
    for(eI in 1:2){
    for(tI in 1:n_t){
      plot(x=1:n_v, y=1:n_v, type="n", ylim=range( c(nu_vt[,,eI,1]+nu_vt[,,eI,2],nu_vt[,,eI,1]-nu_vt[,,eI,2]) ), xlab="Vessel", ylab="Effect", main=Year_Set[tI])
      if(tI==1) mtext( side=2, outer=FALSE, line=2, text=c("Presence/absence","Positive catch rate")[eI])
      for(vI in 1:n_v){
        points( x=vI, y=nu_vt[vI,tI,eI,1])
        lines( x=rep(vI,2), y=c(nu_vt[vI,tI,eI,1]-nu_vt[vI,tI,eI,2],nu_vt[vI,tI,eI,1]+nu_vt[vI,tI,eI,2])) 
      }
    }}
  if( !is.null(FileName_VYplot) ) dev.off()
  
  # Return stuff
  Return = list("nu_vt"=nu_vt)
  return( Return )
}


# Simulate geostatistical data
Geostat_Sim = function(Sim_Settings, MakePlot=TRUE){
  # Initialize GM models
  model_O1 = RMgauss(var=Sim_Settings[['SigmaO1']]^2, scale=Sim_Settings[['Range1']])
  model_E1 = RMgauss(var=Sim_Settings[['SigmaE1']]^2, scale=Sim_Settings[['Range1']])
  model_O2 = RMgauss(var=Sim_Settings[['SigmaO2']]^2, scale=Sim_Settings[['Range2']])
  model_E2 = RMgauss(var=Sim_Settings[['SigmaE2']]^2, scale=Sim_Settings[['Range2']])

  # Generate indices
  s_i = sample(1:nrow(Data_Extrap), size=Sim_Settings[['Nyears']]*Sim_Settings[['Nsamp_per_year']]) #, nrow=Sim_Settings[['Nsamp_per_year']], ncol=Sim_Settings[['Nyears']])
  loc_i = Data_Extrap[s_i,c('E_km','N_km','Lat','Lon')]
  t_i = rep( 1:Sim_Settings[['Nyears']], each=Sim_Settings[['Nsamp_per_year']])
  v_i = rep(rep( 1:4, each=Sim_Settings[['Nsamp_per_year']]/4), Sim_Settings[['Nyears']])

  # Simulate random components
  O1_i = RFsimulate(model=model_O1, x=loc_i[,'E_km'], y=loc_i[,'N_km'])@data[,1] #), nrow=Sim_Settings[['Nsamp_per_year']], ncol=Sim_Settings[['Nyears']])
  O2_i = RFsimulate(model=model_O2, x=loc_i[,'E_km'], y=loc_i[,'N_km'])@data[,1] #), nrow=Sim_Settings[['Nsamp_per_year']], ncol=Sim_Settings[['Nyears']])
  E1_i = E2_i = rep(NA, Sim_Settings[['Nsamp_per_year']]*Sim_Settings[['Nyears']] )
  for(t in 1:Sim_Settings[['Nyears']]){
    E1_i[which(t_i==t)] = RFsimulate(model=model_E1, x=loc_i[which(t_i==t),'E_km'], y=loc_i[which(t_i==t),'N_km'])@data[,1]
    E2_i[which(t_i==t)] = RFsimulate(model=model_E2, x=loc_i[which(t_i==t),'E_km'], y=loc_i[which(t_i==t),'N_km'])@data[,1]
  }
  X_i = Data_Extrap[s_i,c('Depth_km','Depth_km2','Dist_sqrtkm')]
  Vessel_vyc = array( rnorm( n=4*Sim_Settings[['Nyears']]*2, mean=0, sd=Sim_Settings[['SigmaVY1']]), dim=c(4,Sim_Settings[['Nyears']],2))

  # Calculate expected values, and simulate
  P1_i = O1_i + E1_i + as.matrix(X_i)%*%unlist(Sim_Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
  R1_i = plogis( P1_i + Vessel_vyc[v_i[which(t_i==t)],t,1] )
  P2_i = O1_i + E1_i + as.matrix(X_i)%*%unlist(Sim_Settings[c('Depth_km','Depth_km2','Dist_sqrtkm')])
  R2_i = exp( P2_i + Vessel_vyc[v_i[which(t_i==t)],t,2] )
  CPUE_i = rlnorm( n=length(R2_i), meanlog=log(R2_i), sdlog=Sim_Settings[['SigmaM']]) * rbinom( n=length(R1_i), size=1, prob=R1_i )
  Data_Geostat = cbind( "Catch_KG"=CPUE_i, "Year"=t_i, "Vessel"=v_i, "AreaSwept_km2"=1/1e2, "Lat"=loc_i[,'Lat'], "Lon"=loc_i[,'Lon'] )

  # plot data
  if(MakePlot==TRUE){
    f = function(Num) ((Num)-min((Num),na.rm=TRUE))/diff(range((Num),na.rm=TRUE))
    Col = colorRampPalette(colors=c("blue","grey","red"))
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
      png(file=paste(DateFile,Species,"_True_",switch(RespI, "Pres","Pos","Dens","Pos_Rescaled","Dens_Rescaled"),".png",sep=""), width=5*MapSizeRatio['Width(in)'], height=2*MapSizeRatio['Height(in)'], res=200, units='in')
        par(mfrow=c(2,5), mar=c(2,2,0,0))
        for(t in 1:Sim_Settings[['Nyears']]){
          map("worldHires", ylim=Ylim, xlim=Xlim, col="grey90",fill=T, main="", mar=c(0,0,2.5,0),interior=TRUE)
          points(x=Data_Extrap[,'Lon'], y=Data_Extrap[,'Lat'], col=Col(n=10)[ceiling(f(Mat)[,t]*9)+1], cex=0.01)
          #points( x=Data_Geostat[,'Lon'], y=Data_Geostat[,'Lat'], col=Col(n=10)[ceiling(f(Mat)[,t]*9)+1], cex=0.05)
        }
      dev.off()
    }
  }
  Return = list( "Data_Geostat"=Data_Geostat, "True_Index"=True_Index )
  return( Return )
}


