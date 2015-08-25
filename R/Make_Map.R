Make_Map <-
function( Version, TmbData, VesselConfig=c("Vessel"=0,"VesselYear"=0), CovConfig=TRUE, Q_Config=TRUE, RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0), Aniso=TRUE){
  # Local functions
  fixval_fn <- function( fixvalTF ){
    vec = rep(0,length(fixvalTF))
    vec[which(fixvalTF)] = NA
    vec[which(!is.na(vec))] = 1:sum(!is.na(vec))
    vec = factor( vec ) 
    return( vec )
  }
  
  # Create tagged-list in TMB format for fixing parameters
  Map = list()
  # Vessel options
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
  # Configurations of spatial and spatiotemporal error
  if(TmbData[["FieldConfig"]]['Omega1']==0){
    Map[["Omegainput1_s"]] = factor(rep(NA,TmbData$n_s))
    Map[["logetaO1"]] = factor(NA) 
  }
  if(TmbData[["FieldConfig"]]['Epsilon1']==0){
    Map[["Epsiloninput1_st"]] = factor(matrix(NA,nrow=TmbData$n_s,ncol=TmbData$n_t))
    Map[["logetaE1"]] = factor(NA) 
  }
  if(TmbData[["FieldConfig"]]['Omega1']==0 & TmbData[["FieldConfig"]]['Epsilon1']==0) Map[["logkappa1"]] = factor(NA)
  if(TmbData[["FieldConfig"]]['Omega2']==0){
    Map[["Omegainput2_s"]] = factor(rep(NA,TmbData$n_s))
    Map[["logetaO2"]] = factor(NA) 
  }
  if(TmbData[["FieldConfig"]]['Epsilon2']==0){
    Map[["Epsiloninput2_st"]] = factor(matrix(NA,nrow=TmbData$n_s,ncol=TmbData$n_t))
    Map[["logetaE2"]] = factor(NA) 
  }
  if(TmbData[["FieldConfig"]]['Omega2']==0 & TmbData[["FieldConfig"]]['Epsilon2']==0) Map[["logkappa2"]] = factor(NA)
  # Measurement error models
  if(TmbData[["ObsModel"]]%in%c(0,1,2)){
    Map[["logSigmaM"]] = factor( c(1,NA,NA,NA) )
  }
  if(TmbData[["ObsModel"]]%in%c(4,5)){
    Map[["logSigmaM"]] = factor( c(1,NA,2,NA) )
  }
  # Anisotropy
  if(Aniso==0 | all(TmbData[["FieldConfig"]]==0)) Map[['ln_H_input']] = factor( rep(NA,2) )
  
  # Beta1 -- Fixed
  if( RhoConfig["Beta1"]==0 && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Beta_mean1"]] = factor( NA )
    Map[["Beta_rho1"]] = factor( NA )
    Map[["logsigmaB1"]] = factor( NA )
  }
  # Beta1 -- White-noise
  if( RhoConfig["Beta1"]==1 && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Beta_rho1"]] = factor( NA )
  }
  # Beta1 -- Random-walk
  if( RhoConfig["Beta1"]==2 && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Beta_mean1"]] = factor( NA )
    Map[["Beta_rho1"]] = factor( NA )
  }
  # Beta2 -- Fixed
  if( RhoConfig["Beta2"]==0 && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Beta_mean2"]] = factor( NA )
    Map[["Beta_rho2"]] = factor( NA )
    Map[["logsigmaB2"]] = factor( NA )
  }
  # Beta2 -- White-noise
  if( RhoConfig["Beta2"]==1 && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Beta_rho2"]] = factor( NA )
  }
  # Beta2 -- Random-walk
  if( RhoConfig["Beta2"]==2 && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Beta_mean2"]] = factor( NA )
    Map[["Beta_rho2"]] = factor( NA )
  }
  # Epsilon1 -- Fixed OR White-noise OR Random walk
  if( RhoConfig["Epsilon1"]%in%c(0,1,2) && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Epsilon_rho1"]] = factor( NA )
  }
  # Epsilon2 -- Fixed OR White-noise OR Random walk
  if( RhoConfig["Epsilon2"]%in%c(0,1,2) && Version%in%c("geo_index_v3g","geo_index_v3f")){
    Map[["Epsilon_rho2"]] = factor( NA )
  }
  # fix betas and/or epsilons for missing years if betas are fixed-effects
  YearNotInData = !( (1:TmbData$n_t) %in% (unique(TmbData$t_i)+1) ) 
  if( sum(YearNotInData)>0 ){
    # Beta1 -- Fixed
    if( RhoConfig["Beta1"]==0 && Version%in%c("geo_index_v3g","geo_index_v3f")){
      Map[["beta1_t"]] = fixval_fn( fixvalTF=YearNotInData ) 
    }
    # Beta1 -- White-noise
    if( RhoConfig["Beta1"]==1 && Version%in%c("geo_index_v3g","geo_index_v3f")){
      Map[["beta1_t"]] = fixval_fn( fixvalTF=YearNotInData ) 
    }
    # Beta2 -- Fixed
    if( RhoConfig["Beta2"]==0 && Version%in%c("geo_index_v3g","geo_index_v3f")){
      Map[["beta2_t"]] = fixval_fn( fixvalTF=YearNotInData ) 
    }
    # Beta2 -- White-noise
    if( RhoConfig["Beta2"]==1 && Version%in%c("geo_index_v3g","geo_index_v3f")){
      Map[["beta2_t"]] = fixval_fn( fixvalTF=YearNotInData ) 
    }
  }
  
  # Covariates
  Var_j = apply( TmbData[["X_xj"]], MARGIN=2, FUN=var )
  if( Var_j==0 || sum(CovConfig)==0 ){
    Map[["gamma1_j"]] = factor(NA)
    Map[["gamma2_j"]] = factor(NA)
  }
  Var_k = apply( TmbData[["Q_ik"]], MARGIN=2, FUN=var )
  if( Var_k==0 || sum(Q_Config)==0 ){
    Map[["lambda1_k"]] = factor(NA)
    Map[["lambda2_k"]] = factor(NA)
  }
  return(Map)
}
