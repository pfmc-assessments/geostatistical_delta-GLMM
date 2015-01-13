

# Install TMB
# Must be installed from: https://github.com/kaskr/adcomp

# Install INLA
# Must be installed from: http://www.r-inla.org/download

# Install geostatistical delta-GLMM package
library(devtools)
install_github("nwfsc-assess/geostatistical_delta-GLMM")

# Load libraries
library(TMB)
library(INLA)
library(SpatialDeltaGLMM)

# This is where all runs will be located
DateFile = paste(getwd(),'/',Sys.Date(),'/',sep='')
  dir.create(DateFile)

###############
# Settings
###############

  Data_Set = c("NWFSC", "Sim")[1]
  #Sim_Settings = list("Species_Set"=1:100, "Nyears"=10, "Nsamp_per_year"=600, "Depth_km"=-1, "Depth_km2"=-1, "Dist_sqrtkm"=0, "SigmaO1"=0.5, "SigmaO2"=0.5, "SigmaE1"=0.5, "SigmaE2"=0.5, "SigmaVY1"=0.05, "Sigma_VY2"=0.05, "Range1"=1000, "Range2"=500, "SigmaM"=1)
  Version = c("geo_index_v2i", "geo_index_v1h")[1]
  n_x = c(250, 500, 1000, 2000)[3] # Number of stations
  FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence
  CovConfig = c("Depth_km"=0, "Depth_km2"=0, "N_km"=0) # DON'T USE DURING REAL-WORLD DATA FOR ALL SPECIES (IT IS UNSTABLE FOR SOME)
  VesselConfig = c("Vessel"=0, "VesselYear"=1)
  ObsModel = 2  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
  Aniso = 1 # 0=No; 1=Yes
  Kmeans_Config = list( "Locs"=c("Samples","Domain")[1], "nstart"=100, "iter.max"=1e3)     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
  CovConception = FALSE # Include Point Conception as sampling intensity stratum
  Options = c( 0, 0 )  # First slot: Encounter; Second slot: Positive density; 0: Don't ADREPORT() linear predictors; 1: Do ADREPORT() them
  ConvergeTol = c(1,2) # 1:Default; 2:Increased; 3:High

# Compile TMB software
  setwd( system.file("executables", package="SpatialDeltaGLMM") )
  #dyn.unload( dynlib(Version) )
  #file.remove( paste0(Version,c(".o", ".dll")) )
  compile( paste(Version,".cpp",sep="") )
      

################
# Prepare data
################

# Read extrapolation data
  data( extrapolation_data )
  Data_Extrap <- extrapolation_data

# Convert extrapolation-data to an Eastings-Northings coordinate system
  Tmp = cbind('PID'=1,'POS'=1:nrow(Data_Extrap),'X'=Data_Extrap[,'Lon'],'Y'=Data_Extrap[,'Lat'])
  attr(Tmp,"projection") = "LL"
  attr(Tmp,"zone") = "10"
  tmpUTM = convUL(Tmp)                                                         #$
  Data_Extrap = cbind( Data_Extrap, 'E_tmp'=tmpUTM[,'X'], 'N_tmp'=tmpUTM[,'Y'], 'Include'=(Data_Extrap[,'Cowcod']==0 & Data_Extrap[,'Ngdc_m']<(-35)))

# Read or simulate trawl data
  if(Data_Set=="NWFSC"){
    data( Example_NWFSC_shelf_slope_trawl )
    NWFSC_Trawl <- Example_NWFSC_shelf_slope_trawl
    Data_Geostat = cbind( "Catch_KG"=NWFSC_Trawl[,'HAUL_WT_KG'], "Year"=as.numeric(sapply(NWFSC_Trawl[,'PROJECT_CYCLE'],FUN=function(Char){strsplit(as.character(Char)," ")[[1]][2]})), "Vessel"=NWFSC_Trawl[,"VESSEL"], "AreaSwept_km2"=NWFSC_Trawl[,"AREA_SWEPT_HA"]/1e2, "Lat"=NWFSC_Trawl[,'BEST_LAT_DD'], "Lon"=NWFSC_Trawl[,'BEST_LON_DD'])
  }
  if(Data_Set=="Sim"){ #names(Sim_Settings)
    Sim_DataSet = Geostat_Sim(Sim_Settings=Sim_Settings, MakePlot=TRUE)
    Data_Geostat = Sim_DataSet[['Data_Geostat']]
    True_Index = Sim_DataSet[['True_Index']]
  }
  Year_Set = sort(unique(Data_Geostat[,'Year']))

# Convert to an Eastings-Northings coordinate system 
  Tmp = cbind('PID'=1,'POS'=1:nrow(Data_Geostat),'X'=Data_Geostat[,'Lon'],'Y'=Data_Geostat[,'Lat'])
  attr(Tmp,"projection") = "LL"
  attr(Tmp,"zone") = "10"
  tmpUTM = convUL(Tmp)                                                         #$ 
  Data_Geostat = cbind( Data_Geostat, 'E_tmp'=tmpUTM[,'X'], 'N_tmp'=tmpUTM[,'Y'])
  # Find nearest knot in extrapolation data, and define covariates via nearest knot
  NN_Tmp = nn2( data=Data_Extrap[,c('E_tmp','N_tmp')], query=Data_Geostat[,c('E_tmp','N_tmp')], k=1 )
  Data_Geostat = cbind( Data_Geostat, Data_Extrap[NN_Tmp$nn.idx,c('Depth_km','Depth_km2','E_km','N_km')] )

# Calculate k-means centroids (but only once for all species)
  Kmeans = Calc_Kmeans(n_x=n_x, Kmeans_Config=Kmeans_Config, Data_Geostat=Data_Geostat, Data_Extrap=Data_Extrap)
  loc_x = Kmeans$centers

# Calculate areas and average characteristics
  Voronoi = calcVoronoi( xydata=cbind("X"=loc_x[,'E_km'],"Y"=loc_x[,'N_km']), xlim=range(Data_Extrap[,'E_km']), ylim=range(Data_Extrap[,'N_km']))
  NN = nn2( data=loc_x[,c('E_km','N_km')], query=Data_Geostat[,c('E_km','N_km')], k=1 )
  NN_Extrap = nn2( data=loc_x[,c('E_km','N_km')], query=Data_Extrap[,c('E_km','N_km')], k=1 )
  a_x = 4 * tapply(Data_Extrap[,'propInWCGBTS'], INDEX=NN_Extrap$nn.idx, FUN=sum)  # Each cell is 2km x 2km = 4 km^2
  
# Make design matrix (X_xj)
  X_xj = NULL
  for( j in 1:length(CovConfig) ){
    Which = names(CovConfig)[j]
    if(Which=="Depth_km2") X_xj = cbind(X_xj, tapply( Data_Extrap[,"Depth_km"], INDEX=NN_Extrap$nn.idx, FUN=mean, MARGIN=2)^2)
    if(Which!="Depth_km2") X_xj = cbind(X_xj, tapply( Data_Extrap[,Which], INDEX=NN_Extrap$nn.idx, FUN=mean, MARGIN=2))
    colnames(X_xj)[j] = Which
  }
  X_xj = apply(X_xj, MARGIN=2, FUN=function(Vec){(Vec-mean(Vec))/(sd(Vec))})
  # Drop elements from X_xj
  X_xj = X_xj[,which(CovConfig==1)]
  # Add Point Conception if necessary
  if(CovConception==TRUE) X_xj = cbind( X_xj, 'S_of_Concep'=ifelse( tapply( Data_Extrap[,'Lat'], INDEX=NN_Extrap$nn.idx, FUN=mean, MARGIN=2)<34.5, 1, 0) )
  # Make filler matrix if necessary
  if(ncol(X_xj)==0) X_xj = cbind( "Dummy"=rep(0,n_x) )
    
# Make mesh and info for anisotropy
  MeshList = Calc_Anisotropic_Mesh(loc_x=loc_x)

################
# Make and Run TMB model
################

  # Covariates
  TmbData = list("n_i"=nrow(Data_Geostat), "n_s"=MeshList$spde$n.spde, "n_x"=n_x, "n_t"=length(unique(Data_Geostat[,'Year'])), "n_v"=length(unique(Data_Geostat[,'Vessel'])), "n_j"=ncol(X_xj), "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=NN$nn.idx[,1]-1, "t_i"=Data_Geostat[,'Year']-min(Data_Geostat[,'Year']), "a_x"=a_x, "X_xj"=X_xj, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0_inv"=as(diag(1/diag(MeshList$spde$param.inla$M0)),"dgTMatrix"), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2)

  # Parameters
  Parameters = list("ln_H_input"=c(0,0), "beta1_t"=qlogis(tapply(ifelse(TmbData$b_i>0,1,0),INDEX=TmbData$t_i,FUN=mean)), "gamma1_j"=rep(0,TmbData$n_j), "logetaE1"=0, "logetaO1"=0, "logkappa1"=0, "logsigmaV1"=log(1), "logsigmaVT1"=log(1), "nu1_v"=rep(0,TmbData$n_v), "nu1_vt"=matrix(0,nrow=TmbData$n_v,ncol=TmbData$n_t), "Omegainput1_s"=rep(0,TmbData$n_s), "Epsiloninput1_st"=matrix(0,nrow=TmbData$n_s,ncol=TmbData$n_t), "beta2_t"=log(tapply(ifelse(TmbData$b_i>0,TmbData$b_i/TmbData$a_i,NA),INDEX=TmbData$t_i,FUN=mean,na.rm=TRUE)), "gamma2_j"=rep(0,TmbData$n_j), "logetaE2"=0, "logetaO2"=0, "logkappa2"=0, "logsigmaV2"=log(1), "logsigmaVT2"=log(1), "logSigmaM"=c(log(5),qlogis(0.8),log(2)), "nu2_v"=rep(0,TmbData$n_v), "nu2_vt"=matrix(0,nrow=TmbData$n_v,ncol=TmbData$n_t), "Omegainput2_s"=rep(0,TmbData$n_s), "Epsiloninput2_st"=matrix(0,nrow=TmbData$n_s,ncol=TmbData$n_t))

  # Which are random
  Random = c("Epsiloninput1_st", "Omegainput1_s", "Epsiloninput2_st", "Omegainput2_s", "nu1_v", "nu2_v", "nu1_vt", "nu2_vt")

  # Which parameters are turned off
  Map = Make_Map( VesselConfig=VesselConfig, TmbData=TmbData, FieldConfig=FieldConfig, CovConfig=CovConfig, CovConception=CovConception, ObsModel=ObsModel, Aniso=Aniso)

  # Build object                                                              
  dyn.load( paste0(system.file("executables", package="SpatialDeltaGLMM"),"/",dynlib(Version)) )
  if(any(FieldConfig!=0)|any(VesselConfig!=0)){
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, random=Random, hessian=FALSE, map=Map, inner.method="newton")
  }else{
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, hessian=FALSE, map=Map)
  }
  Obj$control <- list(trace=1, parscale=1, REPORT=1, reltol=1e-12, maxit=100)
  
  # Run first time
  Obj$fn(Obj$par)

  # Declare upper and lower bounds for parameter search
  Lower = rep(-50, length(Obj$par))
  Lower[grep("logsigmaV",names(Obj$par))] = log(0.01)
  if( "gamma1" %in% names(Obj$par) ){
    Lower[grep("gamma1",names(Obj$par))] = -20
    Upper[grep("gamma1",names(Obj$par))] = 20
  }
  Upper = rep( 50, length(Obj$par))
  Upper[grep("logtau",names(Obj$par))] = 10   # Version < v2i
  Upper[grep("logeta",names(Obj$par))] = log(1/(1e-2*sqrt(4*pi))) # Version >= v2i: Lower bound on margSD = 1e-4
  Upper[grep("SigmaM",names(Obj$par))] = 10 # ZINB can crash if it gets > 20
  if( "gamma2" %in% names(Obj$par) ){
    Lower[grep("gamma2",names(Obj$par))] = -20
    Upper[grep("gamma2",names(Obj$par))] = 20
  }

  # Change convergence tolerance
  Obj$env$inner.control$step.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol[1]] # Default : 1e-8  # Change in parameters limit inner optimization
  Obj$env$inner.control$tol10 <- c(1e-6,1e-8,1e-12)[ConvergeTol[1]]  # Default : 1e-3     # Change in pen.like limit inner optimization
  Obj$env$inner.control$grad.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol[1]] # # Default : 1e-8  # Maximum gradient limit inner optimization

  # Run model
  Opt = nlminb(start=Obj$par, objective=Obj$fn, gradient=Obj$gr, lower=Lower, upper=Upper, control=list(eval.max=1e4, iter.max=1e4, trace=1, rel.tol=c(1e-8,1e-10,1e-14)[ConvergeTol[2]]))  # , rel.tol=1e-20
    
  # Reports
  Report = Obj$report()                                      
  Sdreport = sdreport(Obj)
  
################
# Make diagnostic plots
################

  # Plot Anisotropy  
  if(Aniso==1){
    PlotAniso_Fn( FileName=paste0(DateFile,"Aniso.png"), Report=Report )
  }

  # Plot surface
  PlotMap_Fn(MappingDetails=list("state", c("Oregon","Washington","California")), Report=Report, MapSizeRatio=c("Height(in)"=4,"Width(in)"=1.55), Xlim=c(-126,-117), Ylim=c(32,49), FileName=paste0(DateFile,"Field_"), Year_Set=Year_Set, Rotate=20, mfrow=c(3,4), mar=c(0,0,2,0), oma=c(3.5,3.5,0,0))

  # Covariate effect
  PlotCov_Fn(Report=Report, NN_Extrap=NN_Extrap, X_xj=X_xj, FileName=paste0(DateFile,"Cov_"))
  
  # Time series measures
  Timeseries_Fn(Report=Report, Year_Set=Year_Set, FileName=paste0(DateFile,"Summaries.png"))
  
  # Positive catch rate Q-Q plot
  Q = QQ_Fn( TmbData=TmbData, Report=Report, FileName_PP=paste0(DateFile,"Posterior_Predictive.jpg"), FileName_Phist=paste0(DateFile,"Posterior_Predictive-Histogram.jpg"), FileName_QQ=paste0(DateFile,"Q-Q_plot.jpg"), FileName_Qhist=paste0(DateFile,"Q-Q_hist.jpg"))

  # Vessel effects
  Return = Vessel_Fn(TmbData=TmbData, Sdreport=Sdreport, FileName_VYplot=paste0(DateFile,"VY-effect.jpg"))

