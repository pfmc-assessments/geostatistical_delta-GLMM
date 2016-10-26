#' Calculate parameter inputs for TMB
#'
#' \code{Param_Fn} generates the \code{parameters} input for \code{TMB::MakeADFun}
#'
#' @param DataList list outputted from \code{SpatialDeltaGLMM::Data_Fn}
#' @inheritParams Data_Fn

#' @return Tagged list containing starting values for all fixed effects (parameters) and random effects (coefficients)
#' \describe{
#'   \item{ln_H_input}{two parameters governing geometric anisotropy (rotation matrix H)}
#'   \item{hyperparameters_z}{parameters for estimating relationships among derived quantities (generally turned off)}
#'   \item{beta1_t}{intercepts for encounter probability}
#'   \item{gamma1_j}{effect of density covariates that are static over time on encounter prob}
#'   \item{gamma1_tp}{effect of density covariates that change over time on encounter prob}
#'   \item{lambda1_k}{effect of catchability covariates on encounter prob}
#'   \item{logetaE1}{reciprocal of pointwise variance in spatio-temporal variation in encounter prob}
#'   \item{logetaO1}{reciprocal of pointwise variance in spatial variation in encounter prob}
#'   \item{logkappa1}{governs decorrelation distance in encounter prob}
#'   \item{logsigmaV1}{log-SD of vessel effects that are static over time on encounter prob}
#'   \item{logsigmaVT1}{log-SD of vessel effects that vary over time on encounter prob}
#'   \item{Beta_mean1}{average intercept of encounter prob (used with RhoConfig options)}
#'   \item{logsigmaB1}{SD for intercept of encounter prob (used with RhoConfig options)}
#'   \item{Beta_rho1}{first-order autoregressive coefficient for intercept of encounter prob (used with RhoConfig options)}
#'   \item{Epsilon_rho1}{first-order autoregressive coefficient for spatio-temporal variation of encounter prob (used with RhoConfig options)}
#'   \item{nu1_v}{vessel effects that are static over time on encounter prob}
#'   \item{nu1_vt}{vessel effects that are static over time on encounter prob}
#'   \item{Omegainput1_s}{Spatial variation in encounter prob}
#'   \item{Epsiloninput1_st}{Spatio-temporal variation in encounter prob}
#'   \item{beta2_t}{intercepts for positive catch-rate}
#'   \item{gamma2_j}{effect of density covariates that are static over time on positive catch-rate}
#'   \item{gamma2_tp}{effect of density covariates that change over time on positive catch-rate}
#'   \item{lambda2_k}{effect of catchability covariates on positive catch-rate}
#'   \item{logetaE2}{reciprocal of pointwise variance in spatio-temporal variation in positive catch-rate}
#'   \item{logetaO2}{reciprocal of pointwise variance in spatial variation in positive catch-rate}
#'   \item{logkappa2}{governs decorrelation distance in positive catch-rate}
#'   \item{logsigmaV2}{log-SD of vessel effects that are static over time on positive catch-rate}
#'   \item{logsigmaVT2}{log-SD of vessel effects that vary over time on positive catch-rate}
#'   \item{Beta_mean2}{average intercept of positive catch-rate (used with RhoConfig options)}
#'   \item{logsigmaB2}{SD for intercept of positive catch-rate (used with RhoConfig options)}
#'   \item{Beta_rho2}{first-order autoregressive coefficient for intercept of positive catch-rate (used with RhoConfig options)}
#'   \item{Epsilon_rho2}{first-order autoregressive coefficient for spatio-temporal variation of positive catch-rate (used with RhoConfig options)}
#'   \item{nu2_v}{vessel effects that are static over time on positive catch-rate}
#'   \item{nu2_vt}{vessel effects that are static over time on positive catch-rate}
#'   \item{Omegainput2_s}{Spatial variation in positive catch-rate}
#'   \item{Epsiloninput2_st}{Spatio-temporal variation in positive catch-rate}
#'   \item{logSigmaM}{variance parameters for positive catch ratesA}
#' }

#' @export
Param_Fn <-
function( Version, DataList, RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0) ){
  # 
  if(Version%in%c("geo_index_v3e","geo_index_v3d","geo_index_v3c","geo_index_v3b","geo_index_v3a","geo_index_v2j","geo_index_v2i")){                                                                                                                                                                                                                                                                                                                                                                         
    Return = list("ln_H_input"=c(0,0), "beta1_t"=qlogis(0.01*0.99*tapply(ifelse(DataList$b_i>0,1,0),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean)), "gamma1_j"=rep(0,DataList$n_j), "lambda1_k"=rep(0,DataList$n_k), "logetaE1"=0, "logetaO1"=0, "logkappa1"=0, "logsigmaV1"=log(1), "logsigmaVT1"=log(1), "nu1_v"=rep(0,DataList$n_v), "nu1_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput1_s"=rep(0,DataList$n_s), "Epsiloninput1_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t), "beta2_t"=log(tapply(ifelse(DataList$b_i>0,DataList$b_i/DataList$a_i,NA),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean,na.rm=TRUE)), "gamma2_j"=rep(0,DataList$n_j), "lambda2_k"=rep(0,DataList$n_k), "logetaE2"=0, "logetaO2"=0, "logkappa2"=0, "logsigmaV2"=log(1), "logsigmaVT2"=log(1), "logSigmaM"=c(log(5),qlogis(0.8),log(2),log(5)), "nu2_v"=rep(0,DataList$n_v), "nu2_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput2_s"=rep(0,DataList$n_s), "Epsiloninput2_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t))
  }
  if(Version%in%c("geo_index_v3i","geo_index_v3h","geo_index_v3g","geo_index_v3f")){
    Return = list("ln_H_input"=c(0,0), "beta1_t"=qlogis(0.01*0.99*tapply(ifelse(DataList$b_i>0,1,0),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean)), "gamma1_j"=rep(0,DataList$n_j), "lambda1_k"=rep(0,DataList$n_k), "logetaE1"=0, "logetaO1"=0, "logkappa1"=0, "logsigmaV1"=log(1), "logsigmaVT1"=log(1), "Beta_mean1"=0, "logsigmaB1"=log(1), "Beta_rho1"=0, "Epsilon_rho1"=0, "nu1_v"=rep(0,DataList$n_v), "nu1_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput1_s"=rep(0,DataList$n_s), "Epsiloninput1_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t), "beta2_t"=log(tapply(ifelse(DataList$b_i>0,DataList$b_i/DataList$a_i,NA),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean,na.rm=TRUE)), "gamma2_j"=rep(0,DataList$n_j), "lambda2_k"=rep(0,DataList$n_k), "logetaE2"=0, "logetaO2"=0, "logkappa2"=0, "logsigmaV2"=log(1), "logsigmaVT2"=log(1), "Beta_mean2"=0, "logsigmaB2"=log(1), "Beta_rho2"=0, "Epsilon_rho2"=0, "logSigmaM"=c(log(5),qlogis(0.8),log(2),log(5)), "nu2_v"=rep(0,DataList$n_v), "nu2_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput2_s"=rep(0,DataList$n_s), "Epsiloninput2_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t))
  }
  if(Version%in%c("geo_index_v3k","geo_index_v3j")){
    Return = list("ln_H_input"=c(0,0), "hyperparameters_z"=rep(0,3), "beta1_t"=qlogis(0.01*0.99*tapply(ifelse(DataList$b_i>0,1,0),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean)), "gamma1_j"=rep(0,DataList$n_j), "lambda1_k"=rep(0,DataList$n_k), "logetaE1"=0, "logetaO1"=0, "logkappa1"=0, "logsigmaV1"=log(1), "logsigmaVT1"=log(1), "Beta_mean1"=0, "logsigmaB1"=log(1), "Beta_rho1"=0, "Epsilon_rho1"=0, "nu1_v"=rep(0,DataList$n_v), "nu1_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput1_s"=rep(0,DataList$n_s), "Epsiloninput1_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t), "beta2_t"=log(tapply(ifelse(DataList$b_i>0,DataList$b_i/DataList$a_i,NA),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean,na.rm=TRUE)), "gamma2_j"=rep(0,DataList$n_j), "lambda2_k"=rep(0,DataList$n_k), "logetaE2"=0, "logetaO2"=0, "logkappa2"=0, "logsigmaV2"=log(1), "logsigmaVT2"=log(1), "Beta_mean2"=0, "logsigmaB2"=log(1), "Beta_rho2"=0, "Epsilon_rho2"=0, "logSigmaM"=c(log(5),qlogis(0.8),log(2),log(5)), "nu2_v"=rep(0,DataList$n_v), "nu2_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput2_s"=rep(0,DataList$n_s), "Epsiloninput2_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t))
  }
  if(Version%in%c("geo_index_v4b","geo_index_v4a","geo_index_v3n","geo_index_v3m","geo_index_v3l")){
    Return = list("ln_H_input"=c(0,0), "hyperparameters_z"=rep(0,3), "beta1_t"=qlogis(0.01*0.99*tapply(ifelse(DataList$b_i>0,1,0),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean)), "gamma1_j"=rep(0,DataList$n_j), "gamma1_tp"=matrix(0,nrow=DataList$n_t,ncol=DataList$n_p), "lambda1_k"=rep(0,DataList$n_k), "logetaE1"=0, "logetaO1"=0, "logkappa1"=0, "logsigmaV1"=log(1), "logsigmaVT1"=log(1), "Beta_mean1"=0, "logsigmaB1"=log(1), "Beta_rho1"=0, "Epsilon_rho1"=0, "nu1_v"=rep(0,DataList$n_v), "nu1_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput1_s"=rep(0,DataList$n_s), "Epsiloninput1_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t), "beta2_t"=log(tapply(ifelse(DataList$b_i>0,DataList$b_i/DataList$a_i,NA),INDEX=factor(DataList$t_i,levels=1:DataList$n_t),FUN=mean,na.rm=TRUE)), "gamma2_j"=rep(0,DataList$n_j), "gamma2_tp"=matrix(0,nrow=DataList$n_t,ncol=DataList$n_p), "lambda2_k"=rep(0,DataList$n_k), "logetaE2"=0, "logetaO2"=0, "logkappa2"=0, "logsigmaV2"=log(1), "logsigmaVT2"=log(1), "Beta_mean2"=0, "logsigmaB2"=log(1), "Beta_rho2"=0, "Epsilon_rho2"=0, "logSigmaM"=c(log(5),qlogis(0.8),log(2),log(5)), "nu2_v"=rep(0,DataList$n_v), "nu2_vt"=matrix(0,nrow=DataList$n_v,ncol=DataList$n_t), "Omegainput2_s"=rep(0,DataList$n_s), "Epsiloninput2_st"=matrix(0,nrow=DataList$n_s,ncol=DataList$n_t))
  }
  # If either beta or epsilon is a random-walk process, fix starting value at 1
  if( "Beta_rho1"%in%names(Return) && RhoConfig[["Beta1"]]==2 ) Return[["Beta_rho1"]] = 1
  if( "Beta_rho2"%in%names(Return) && RhoConfig[["Beta2"]]==2 ) Return[["Beta_rho2"]] = 1
  if( "Epsilon_rho1"%in%names(Return) && RhoConfig[["Epsilon1"]]==2 ) Return[["Epsilon_rho1"]] = 1
  if( "Epsilon_rho2"%in%names(Return) && RhoConfig[["Epsilon2"]]==2 ) Return[["Epsilon_rho2"]] = 1
  # replace missing values function
  tmpfn = function( vec ){
    Return = ifelse( abs(vec)==Inf, NA, vec)
    return( ifelse(is.na(Return),mean(Return,na.rm=TRUE),Return) )
  }
  Return[["beta1_t"]] = tmpfn( Return[["beta1_t"]] )
  Return[["beta2_t"]] = tmpfn( Return[["beta2_t"]] )
  # Error messages
  if( any(sapply(Return, FUN=function(num){any(is.na(num))})) ) stop("Some parameter is NA")
  # Return tagged list
  return( Return )
}
