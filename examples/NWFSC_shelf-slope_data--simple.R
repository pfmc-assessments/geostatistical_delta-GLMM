

# Install TMB
# Must be installed from: https://github.com/kaskr/adcomp

# Install INLA
# Must be installed from: http://www.r-inla.org/download

# Install geostatistical delta-GLMM package
library(devtools)
install_github("nwfsc-assess/geostatistical_delta-GLMM") # This is the developement version.  Please check GitHub for the latest release number.

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
  Version = "geo_index_v3b"
  n_x = c(250, 500, 1000, 2000)[3] # Number of stations
  FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence
  CovConfig = c("SST"=0) # DON'T USE DURING REAL-WORLD DATA FOR ALL SPECIES (IT IS UNSTABLE FOR SOME)
  Q_Config = c("Pass"=0)
  VesselConfig = c("Vessel"=0, "VesselYear"=1)
  ObsModel = 2  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
  Aniso = 1 # 0=No; 1=Yes
  Kmeans_Config = list( "Locs"=c("Samples","Domain")[1], "nstart"=100, "iter.max"=1e3)     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
  ConvergeTol = c(1,2) # 1:Default; 2:Increased; 3:High

# Decide on strata for use when calculating indices
  # In this case, it will calculate a coastwide index, and also a separate index for each state (although the state lines are approximate)
strata.limits <- nwfscDeltaGLM::readIn(ncol=5,nlines=5)
  STRATA  NLat SLat MinDepth MaxDepth
  Coastwide 49.0 32.0  55       1280
  CA        42.0 32.0  55       1280
  OR        46.0 42.0  55       1280
  WA        49.0 46.0  55       1280

# Compile TMB software
  setwd( system.file("executables", package="SpatialDeltaGLMM") )
  compile( paste(Version,".cpp",sep="") )
      

################
# Prepare data
# (THIS WILL VARY FOR DIFFERENT DATA SETS) 
################

# Read extrapolation data
  data( extrapolation_data )
  Data_Extrap <- extrapolation_data

# Augment with strata for each extrapolation cell
  Tmp = cbind("BEST_DEPTH_M"=(-1000)*Data_Extrap[,'Depth_km'], "BEST_LAT_DD"=Data_Extrap[,'Lat'], "propInWCGBTS"=Data_Extrap[,'propInWCGBTS'])
  a_el = as.data.frame(matrix(NA, nrow=nrow(Data_Extrap), ncol=nrow(strata.limits)))
  for(l in 1:ncol(a_el)){
    a_el[,l] = apply(Tmp , MARGIN=1, FUN=nwfscDeltaGLM::strata.fn, Strata.df=strata.limits[l,])
    a_el[,l] = ifelse( is.na(a_el[,l]), 0, 4*Data_Extrap[,'propInWCGBTS'])
  }

# Convert extrapolation-data to an Eastings-Northings coordinate system
  Tmp = cbind('PID'=1,'POS'=1:nrow(Data_Extrap),'X'=Data_Extrap[,'Lon'],'Y'=Data_Extrap[,'Lat'])
  attr(Tmp,"projection") = "LL"
  attr(Tmp,"zone") = "10"
  tmpUTM = convUL(Tmp)                                                         #$
  Data_Extrap = cbind( Data_Extrap, 'E_tmp'=tmpUTM[,'X'], 'N_tmp'=tmpUTM[,'Y'], 'Include'=(Data_Extrap[,'Cowcod']==0 & Data_Extrap[,'Ngdc_m']<(-35)))

# Read or simulate trawl data
  if(Data_Set=="Canary_rockfish"){
    data( WCGBTS_Canary_example )
    NWFSC_Trawl <- WCGBTS_Canary_example
    Data_Geostat = cbind( "Catch_KG"=NWFSC_Trawl[,'HAUL_WT_KG'], "Year"=as.numeric(sapply(NWFSC_Trawl[,'PROJECT_CYCLE'],FUN=function(Char){strsplit(as.character(Char)," ")[[1]][2]})), "Vessel"=NWFSC_Trawl[,"VESSEL"], "AreaSwept_km2"=NWFSC_Trawl[,"AREA_SWEPT_HA"]/1e2, "Lat"=NWFSC_Trawl[,'BEST_LAT_DD'], "Lon"=NWFSC_Trawl[,'BEST_LON_DD'], "Pass"=NWFSC_Trawl[,'PASS']-1.5)
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
  setwd( DateFile ) 
  Kmeans = Calc_Kmeans(n_x=n_x, Kmeans_Config=Kmeans_Config, Data_Geostat=Data_Geostat, Data_Extrap=Data_Extrap)
  loc_x = Kmeans$centers

# Calculate areas and average characteristics
  Voronoi = calcVoronoi( xydata=cbind("X"=loc_x[,'E_km'],"Y"=loc_x[,'N_km']), xlim=range(Data_Extrap[,'E_km']), ylim=range(Data_Extrap[,'N_km']))
  NN = nn2( data=loc_x[,c('E_km','N_km')], query=Data_Geostat[,c('E_km','N_km')], k=1 )
  NN_Extrap = nn2( data=loc_x[,c('E_km','N_km')], query=Data_Extrap[,c('E_km','N_km')], k=1 )
  a_xl = matrix(NA, ncol=ncol(a_el), nrow=n_x)
  for(l in 1:ncol(a_xl)) a_xl[,l] = tapply(a_el[,l], INDEX=NN_Extrap$nn.idx, FUN=sum)
  
# Make design matrix (X_xj)
  if( sum(CovConfig)==0 ){
    X_xj = cbind( "Dummy"=rep(0,n_x) )
  }else{
    ## NOT IMPLEMENTED IN EXAMPLE ##  
  }
    
# Make catchability matrix (Q_i)
  if( sum(Q_Config)==0 ){
    Q_ik = matrix(0, ncol=1, nrow=nrow(Data_Geostat))
  }else{
    Q_ik = as.matrix(Data_Geostat[,names(Q_Config)[which(Q_Config==1)],drop=FALSE])
  }
    
# Make mesh and info for anisotropy
  MeshList = Calc_Anisotropic_Mesh(loc_x=loc_x)

################
# Make and Run TMB model
# (THIS WILL BE SIMILAR FOR EVERY DATA SET) 
################

  # Data
  TmbData = Data_Fn("Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=NN$nn.idx[,1]-1, "t_i"=Data_Geostat[,'Year']-min(Data_Geostat[,'Year']), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "MeshList"=MeshList)

  # Parameters
  Parameters = list("ln_H_input"=c(0,0), "beta1_t"=qlogis(tapply(ifelse(TmbData$b_i>0,1,0),INDEX=TmbData$t_i,FUN=mean)), "gamma1_j"=rep(0,TmbData$n_j), "lambda1_k"=rep(0,TmbData$n_k), "logetaE1"=0, "logetaO1"=0, "logkappa1"=0, "logsigmaV1"=log(1), "logsigmaVT1"=log(1), "nu1_v"=rep(0,TmbData$n_v), "nu1_vt"=matrix(0,nrow=TmbData$n_v,ncol=TmbData$n_t), "Omegainput1_s"=rep(0,TmbData$n_s), "Epsiloninput1_st"=matrix(0,nrow=TmbData$n_s,ncol=TmbData$n_t), "beta2_t"=log(tapply(ifelse(TmbData$b_i>0,TmbData$b_i/TmbData$a_i,NA),INDEX=TmbData$t_i,FUN=mean,na.rm=TRUE)), "gamma2_j"=rep(0,TmbData$n_j), "lambda2_k"=rep(0,TmbData$n_k), "logetaE2"=0, "logetaO2"=0, "logkappa2"=0, "logsigmaV2"=log(1), "logsigmaVT2"=log(1), "logSigmaM"=c(log(5),qlogis(0.8),log(2),log(5)), "nu2_v"=rep(0,TmbData$n_v), "nu2_vt"=matrix(0,nrow=TmbData$n_v,ncol=TmbData$n_t), "Omegainput2_s"=rep(0,TmbData$n_s), "Epsiloninput2_st"=matrix(0,nrow=TmbData$n_s,ncol=TmbData$n_t))

  # Which are random
  Random = c("Epsiloninput1_st", "Omegainput1_s", "Epsiloninput2_st", "Omegainput2_s", "nu1_v", "nu2_v", "nu1_vt", "nu2_vt")

  # Which parameters are turned off
  Map = Make_Map( VesselConfig=VesselConfig, TmbData=TmbData, FieldConfig=FieldConfig, CovConfig=CovConfig, ObsModel=ObsModel, Aniso=Aniso)

  # Build object                                                              
  dyn.load( paste0(system.file("executables", package="SpatialDeltaGLMM"),"/",dynlib(Version)) )
  if(any(FieldConfig!=0)|any(VesselConfig!=0)){
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, random=Random, hessian=FALSE, map=Map, inner.method="newton")
  }else{
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, hessian=FALSE, map=Map)
  }
  Obj$control <- list(trace=1, parscale=1, REPORT=1, reltol=1e-12, maxit=100)
  
  # Run first time -- marginal likelihood
  Obj$fn(Obj$par)
  # Run first time -- gradient with respect to fixed effects
  Obj$gr(Obj$par)

  # Declare upper and lower bounds for parameter search
  Lower = rep(-50, length(Obj$par))
  Lower[grep("logsigmaV",names(Obj$par))] = log(0.01)
  Upper = rep( 50, length(Obj$par))
  Upper[grep("logtau",names(Obj$par))] = 10   # Version < v2i
  Upper[grep("logeta",names(Obj$par))] = log(1/(1e-2*sqrt(4*pi))) # Version >= v2i: Lower bound on margSD = 1e-4
  Upper[grep("SigmaM",names(Obj$par))] = 10 # ZINB can crash if it gets > 20
  if( "gamma1" %in% names(Obj$par) ){
    Lower[grep("gamma1",names(Obj$par))] = -20
    Upper[grep("gamma1",names(Obj$par))] = 20
  }
  if( "gamma2" %in% names(Obj$par) ){
    Lower[grep("gamma2",names(Obj$par))] = -20
    Upper[grep("gamma2",names(Obj$par))] = 20
  }
  if( "lambda1" %in% names(Obj$par) ){
    Lower[grep("lambda1",names(Obj$par))] = -20
    Upper[grep("lambda1",names(Obj$par))] = 20
  }
  if( "lambda2" %in% names(Obj$par) ){
    Lower[grep("lambda2",names(Obj$par))] = -20
    Upper[grep("lambda2",names(Obj$par))] = 20
  }

  # Change convergence tolerance
  Obj$env$inner.control$step.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol[1]] # Default : 1e-8  # Change in parameters limit inner optimization
  Obj$env$inner.control$tol10 <- c(1e-6,1e-8,1e-12)[ConvergeTol[1]]  # Default : 1e-3     # Change in pen.like limit inner optimization
  Obj$env$inner.control$grad.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol[1]] # # Default : 1e-8  # Maximum gradient limit inner optimization

  # Run model
  Opt = nlminb(start=Obj$par, objective=Obj$fn, gradient=Obj$gr, lower=Lower, upper=Upper, control=list(eval.max=1e4, iter.max=1e4, trace=1, rel.tol=c(1e-8,1e-10,1e-14)[ConvergeTol[2]]))  # , rel.tol=1e-20
  Opt[["final_diagnostics"]] = cbind( "Name"=names(Opt$par), "Lwr"=Lower, "Est"=Opt$par, "Upr"=Upper, "Gradient"=Obj$gr(Opt$par) )
    
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

