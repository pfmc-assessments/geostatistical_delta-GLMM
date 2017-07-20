
#' Simulate data
#'
#' \code{Geostat_Sim} simulates data for use when testing \code{SpatialDeltaGLMM}
#'
#' @param Sim_Settings an optional tagged list for specifying input parameters (see function code to determine settings)
#' @inheritParams Spatial_Information_Fn
#' @param Data_Geostat A data frame with column headers \code{c('Lon','Lat','Year','Vessel','AreaSwept_km2')} containing sample design to mimic
#' @param standardize_fields Boolean, whether to ensure that random fields have sample mean and standard deviation equal to their inputted values

#' @return Return Tagged list of output
#' \describe{
#'   \item{Data_Geostat}{Simulated data for analysis}
#'   \item{B_tl}{True biomass for each year and stratum}
#' }

#' @examples
#' ## Do not run (will be slow, due to simulating fine-scale spatial variation for many sites):
#' ##
#' ## # Prepare inputs
#' ## Database = FishData::download_catch_rates( survey="Eastern_Bering_Sea", species_set="Gadus chalcogrammus", error_tol=0.01 )
#' ## Data_Geostat = data.frame("Lat"=Database[,'Lat'], "Lon"=Database[,'Long'], "Year"=Database[,'Year'], "Vessel"="missing", "AreaSwept_km2"=1 )
#' ## Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region="Eastern_Bering_Sea", strata.limits=data.frame( 'STRATA'="All_areas") )
#' ##
#' ## # Use function
#' ## SimList = SpatialDeltaGLMM::Geostat_Sim(Sim_Settings=list(), Extrapolation_List, Data_Geostat=Data_Geostat )
#' ## Data_Sim = SimList$Data_Geostat

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
  Settings = list("beta1_mean"=0, "beta2_mean"=0, "beta1_slope"=0, "beta2_slope"=0, "beta1_sd"=0, "beta2_sd"=0, "Nyears"=10, "Nsamp_per_year"=600,
    "Depth1_km"=0, "Depth1_km2"=0, "Dist1_sqrtkm"=0, "Depth2_km"=0, "Depth2_km2"=0, "Dist2_sqrtkm"=0,
    "SigmaO1"=0.5, "SigmaO2"=0.5, "SigmaE1"=0.1, "SigmaE2"=0.1, "SigmaV1"=0, "SigmaV2"=0, "SigmaVY1"=0, "SigmaVY2"=0,
    "Range1"=1000, "Range2"=500, "SigmaM"=1, "ObsModel"=c(2,0),
    "Nages"=1, "M"=Inf, "K"=Inf, "Linf"=1, "W_alpha"=1, "W_beta"=3, "Selex_A50_mean"=0, "Selex_A50_sd"=0, "Selex_Aslope"=Inf )

  # Replace defaults with provided values (if any)
  for( i in seq_len(length(Sim_Settings)) ){
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
    w_i = rep(0.01, length(s_i))
  }else{
    NN_domain = RANN::nn2( data=Extrapolation_List$Data_Extrap[,c('Lon','Lat')], query=Data_Geostat[,c('Lon','Lat')], k=1)
    s_i = NN_domain$nn.idx[,1]
    t_i = Data_Geostat[,'Year'] - min(Data_Geostat[,'Year']) + 1
    v_i = as.numeric(factor(Data_Geostat[,'Vessel']))
    w_i = Data_Geostat[,'AreaSwept_km2']
  }

  # Duplicate locations if Nages>1
  if( Settings[["Nages"]]>=2 ){
    a_i = rep( 1:Settings[["Nages"]], times=length(s_i) )
    s_i = rep( s_i, each=Settings[["Nages"]] )
    t_i = rep( t_i, each=Settings[["Nages"]] )
    v_i = rep( v_i, each=Settings[["Nages"]] )
    w_i = rep( w_i, each=Settings[["Nages"]] )
  }else{
    a_i = rep( 1, times=length(s_i) )
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

  # Calculate covariate effect
  eta1_s = as.matrix(X_sj) %*% unlist(Settings[c('Depth1_km','Depth1_km2','Dist1_sqrtkm')])
  eta2_s = as.matrix(X_sj) %*% unlist(Settings[c('Depth2_km','Depth2_km2','Dist2_sqrtkm')])

  # If Nages==1, then simulate as biomass-dynamic model
  if( Settings[["Nages"]]==1 ){
    # Calculate expected values
    P1_st = P2_st = matrix(NA, nrow=nrow(loc_s), ncol=max(t_i) )
    for(t in 1:max(t_i)){
      P1_st[,t] = beta1_t[t] + O1_s + E1_st[,t] + eta1_s
      P2_st[,t] = beta2_t[t] + O2_s + E2_st[,t] + eta2_s
    }

    # Calculate linear predictors
    P1_i = P1_st[cbind(s_i,t_i)] + Vessel1_vy[cbind(v_i,t_i)] + Vessel1_v[v_i]
    P2_i = P2_st[cbind(s_i,t_i)] + Vessel2_vy[cbind(v_i,t_i)] + Vessel2_v[v_i]

    # Calculate true spatio-temporal biomass and annual abundance
    if( Settings[['ObsModel']][2]==0 ){
      B_ast = array( plogis(P1_st) * exp(P2_st) * outer(Extrapolation_List$Area_km2_x,rep(1,max(t_i))), dim=c(1,dim(P1_st)) )
    }
    if( Settings[['ObsModel']][2] %in% c(1,2) ){
      B_ast = array( exp(P1_st) * exp(P2_st) * outer(Extrapolation_List$Area_km2_x,rep(1,max(t_i))), dim=c(1,dim(P2_st)) )
    }
    B_st = B_ast[1,,]

    # Objects specific to Nages==1
    Return = list( "P1_st"=P1_st, "P2_st"=P2_st )

    # Calculate true spatio-temporal biomass and annual abundance
    if( Settings[['ObsModel']][2]==0 ){
      B_st = plogis(P1_st) * exp(P2_st) * outer(Extrapolation_List$Area_km2_x,rep(1,max(t_i)))
    }
    if( Settings[['ObsModel']][2] %in% c(1,2) ){
      B_st = exp(P1_st) * exp(P2_st) * outer(Extrapolation_List$Area_km2_x,rep(1,max(t_i)))
    }
  }

  # If Nages==1, then simulate as biomass-dynamic model
  if( Settings[["Nages"]]>=2 ){
    if( !(Settings[['ObsModel']][2] %in% c(1,2)) ) stop("If using age-structured simulation, please use 'Settings[['ObsModel']][2]=1'")

    # Deviations in initial age-structure
    E1_sa = matrix(NA, nrow=nrow(loc_s), ncol=Settings[["Nages"]] )
    for(a in 2:Settings[["Nages"]]){
      E1_sa[,a] = RFsim(model=model_E1, x=loc_s[,'E_km'], y=loc_s[,'N_km'], standardize=standardize_fields)
      message( "Finished variation for age ", a, " in year 1" )
    }

    # Biomass at age
    L_a = Settings[["Linf"]] * (1 - exp(-Settings[["K"]] * 1:Settings[["Nages"]]) )
    W_a = Settings[["W_alpha"]] * L_a^Settings[["W_beta"]]

    # Selectivity at age
    Selex_A50_v = rnorm( max(v_i), mean=Settings[["Selex_A50_mean"]], sd=Settings[["Selex_A50_sd"]] )
    Selex_av = NULL
    for( v in 1:max(v_i)) Selex_av = cbind( Selex_av, plogis(1:Settings[["Nages"]], location=Selex_A50_v[v], scale=Settings[["Selex_Aslope"]]) )

    # Numbers-density at age (N_ast) and Individual weight at age (W_ast) for age 1
    N_ast = W_ast = array(NA, dim=c(Settings[["Nages"]],nrow(loc_s),max(t_i)) )
    for(t in 1:max(t_i)){
      # Individual weight at age
      W_ast[,,t] = W_a %o% exp( beta2_t[t] + O2_s + E2_st[,t] + eta2_s )
      # Numbers-density at age
      N_ast[1,,t] = exp( beta1_t[t] + O1_s + E1_st[,t] + eta1_s )
      for( a in 2:Settings[["Nages"]] ){
        # Initial age-structure
        if( t==1 ){
          N_ast[a,,t] = exp( beta1_t[t] + O1_s + E1_sa[,a] + eta1_s - Settings[["M"]]*(a-1)  )
        }
        if( t>=2 ){
          N_ast[a,,t] = exp(-Settings[["M"]]) * N_ast[a-1,,t-1]
        }
      }
    }

    # Calculate linear predictors
    P1_i = log(Selex_av[cbind(a_i,v_i)] * N_ast[cbind(a_i,s_i,t_i)]) + Vessel1_vy[cbind(v_i,t_i)] + Vessel1_v[v_i]
    P2_i = log(W_ast[cbind(a_i,s_i,t_i)]) + Vessel2_vy[cbind(v_i,t_i)] + Vessel2_v[v_i]

    # Calculate true AVAILABLE spatio-temporal biomass and annual abundance
    Selex_a = plogis(1:Settings[["Nages"]], location=Settings[["Selex_A50_mean"]], scale=Settings[["Selex_Aslope"]])
    B_ast = N_ast * W_ast * (Selex_a %o% Extrapolation_List$Area_km2_x %o% rep(1,max(t_i)))
    B_st = apply( B_ast, MARGIN=2:3, FUN=sum )
    B_at = apply( B_ast, MARGIN=c(1,3), FUN=sum )

    # Objects specific to Nages>=2
    Return = list( "L_a"=L_a, "W_a"=W_a, "Selex_av"=Selex_av, "E1_sa"=E1_sa, "N_ast"=N_ast, "W_ast"=W_ast, "B_ast"=B_ast, "B_at"=B_at )
  }

  # Calculate linked-predictors
  if( Settings[['ObsModel']][2]==0 ){
    R1_i = plogis( P1_i )
    R2_i = w_i * exp( P2_i )
  }
  if( Settings[['ObsModel']][2]==1 ){
    R1_i = 1 - exp( -1*w_i*exp(P1_i) )
    R2_i = w_i*exp(P1_i) / R1_i * exp(P2_i)
  }
  if( Settings[['ObsModel']][2]==2 ){
    R1_i = w_i * exp(P1_i)
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
    for(i in 1:length(b_i)) b_i[i] = rPoisGam( n=1, shape=Settings[['SigmaM']], scale=R2_i[i], intensity=R1_i[i] )
  }
  if( Settings[['ObsModel']][1]==10 ){
    if( Settings[['ObsModel']][2]!=2 ) stop("Must use 'ObsModel=c(10,2)'")
    b_i = rep(NA,length(R1_i))                            # R1_i(i)*R2_i(i), R1_i(i), invlogit(SigmaM(c_i(i),0))+1.0
    for(i in 1:length(b_i)) b_i[i] = tweedie::rtweedie(n=1, mu=R1_i[i]*R2_i[i], phi=R1_i[i], power=plogis(Settings[['SigmaM']])+1.0)
  }
  Data_Geostat = cbind( "Catch_KG"=b_i, "Year"=t_i, "Vessel"=v_i, "Age"=a_i, "AreaSwept_km2"=w_i, "Lat"=loc_i[,'Lat'], "Lon"=loc_i[,'Lon'] )

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

  # Timing
  time_for_simulation = Sys.time()-start_time
  message( "Total time: ", time_for_simulation )

  # Objects shared by both
  Return = c( Return, list("Data_Geostat"=data.frame(Data_Geostat), "B_tl"=B_tl, "B_st"=B_st, "COG_tm"=COG_tm, "beta1_t"=beta1_t,
    "beta2_t"=beta2_t, "O1_s"=O1_s, "O2_s"=O2_s, "E1_st"=E1_st, "Vessel1_vy"=Vessel1_vy, "Vessel2_vy"=Vessel2_vy, "eta1_s"=eta1_s, "eta2_s"=eta2_s,
    "Vessel1_v"=Vessel1_v, "Vessel2_v"=Vessel2_v, "E2_st"=E2_st, "s_i"=s_i, "t_i"=t_i, "a_i"=a_i, "w_i"=w_i,
    "P1_i"=P1_i, "P2_i"=P2_i, "R1_i"=R1_i, "R2_i"=R2_i,
    "time_for_simulation"=time_for_simulation, "Sim_Settings"=Settings) )
  if( exists("b1_i")) Return = c(Return, list("b1_i"=b1_i, "b2_i"=b2_i))
  return( Return )
}
