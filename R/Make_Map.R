Make_Map <-
function( VesselConfig, TmbData, FieldConfig, CovConfig, CovConception=FALSE, ObsModel, Aniso){
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
  if( sum(Q_Config)==0 ){
    Map[["lambda1_k"]] = factor(NA)
    Map[["lambda2_k"]] = factor(NA)
  }
  if(ObsModel%in%c(0,1,2)){
    Map[["logSigmaM"]] = factor( c(1,NA,NA,NA) )
  }
  if(ObsModel%in%c(4,5)){
    Map[["logSigmaM"]] = factor( c(1,NA,2,NA) )
  }
  if(Aniso==0 | all(FieldConfig==0)) Map[['ln_H_input']] = factor( rep(NA,2) )
  return(Map)
}
