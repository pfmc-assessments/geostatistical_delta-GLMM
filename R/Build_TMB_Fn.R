Build_TMB_Fn <-
function( TmbData, TmbFile, Version, VesselConfig, CovConfig, Aniso, ConvergeTol=2 ){

  # Parameters
  Parameters = list("ln_H_input"=c(0,0), "beta1_t"=qlogis(tapply(ifelse(TmbData$b_i>0,1,0),INDEX=TmbData$t_i,FUN=mean)), "gamma1_j"=rep(0,TmbData$n_j), "lambda1_k"=rep(0,TmbData$n_k), "logetaE1"=0, "logetaO1"=0, "logkappa1"=0, "logsigmaV1"=log(1), "logsigmaVT1"=log(1), "nu1_v"=rep(0,TmbData$n_v), "nu1_vt"=matrix(0,nrow=TmbData$n_v,ncol=TmbData$n_t), "Omegainput1_s"=rep(0,TmbData$n_s), "Epsiloninput1_st"=matrix(0,nrow=TmbData$n_s,ncol=TmbData$n_t), "beta2_t"=log(tapply(ifelse(TmbData$b_i>0,TmbData$b_i/TmbData$a_i,NA),INDEX=TmbData$t_i,FUN=mean,na.rm=TRUE)), "gamma2_j"=rep(0,TmbData$n_j), "lambda2_k"=rep(0,TmbData$n_k), "logetaE2"=0, "logetaO2"=0, "logkappa2"=0, "logsigmaV2"=log(1), "logsigmaVT2"=log(1), "logSigmaM"=c(log(5),qlogis(0.8),log(2),log(5)), "nu2_v"=rep(0,TmbData$n_v), "nu2_vt"=matrix(0,nrow=TmbData$n_v,ncol=TmbData$n_t), "Omegainput2_s"=rep(0,TmbData$n_s), "Epsiloninput2_st"=matrix(0,nrow=TmbData$n_s,ncol=TmbData$n_t))

  # Which are random
  Random = c("Epsiloninput1_st", "Omegainput1_s", "Epsiloninput2_st", "Omegainput2_s", "nu1_v", "nu2_v", "nu1_vt", "nu2_vt")

  # Which parameters are turned off
  Map = Make_Map( TmbData=TmbData, VesselConfig=VesselConfig, CovConfig=CovConfig, Aniso=Aniso)

  # Build object
  dyn.load( paste0(TmbFile,"/",dynlib(Version)) )
  if(any(FieldConfig!=0)|any(VesselConfig!=0)){
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, random=Random, hessian=FALSE, map=Map, inner.method="newton")
  }else{
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, hessian=FALSE, map=Map)
  }
  Obj$control <- list(trace=1, parscale=1, REPORT=1, reltol=1e-12, maxit=100)

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
  Obj$env$inner.control$step.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol] # Default : 1e-8  # Change in parameters limit inner optimization
  Obj$env$inner.control$tol10 <- c(1e-6,1e-8,1e-12)[ConvergeTol]  # Default : 1e-3     # Change in pen.like limit inner optimization
  Obj$env$inner.control$grad.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol] # # Default : 1e-8  # Maximum gradient limit inner optimization

  # Return stuff
  Return = list("Obj"=Obj, "Upper"=Upper, "Lower"=Lower)
  return( Return )
}
