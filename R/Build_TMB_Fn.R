Build_TMB_Fn <-
function( TmbData, TmbDir, Version, VesselConfig, CovConfig, Q_Config, RhoConfig=c("Epsilon1"=0,"Epsilon2"=0), Aniso, ConvergeTol=2, Parameters=NULL ){

  # Parameters
  if( is.null(Parameters) ) Parameters = Param_Fn( Version=Version, DataList=TmbData )

  # Which are random
  Random = c("Epsiloninput1_st", "Omegainput1_s", "Epsiloninput2_st", "Omegainput2_s", "nu1_v", "nu2_v", "nu1_vt", "nu2_vt")

  # Which parameters are turned off
  Map = Make_Map( Version=Version, TmbData=TmbData, VesselConfig=VesselConfig, CovConfig=CovConfig, Q_Config=Q_Config, RhoConfig=RhoConfig, Aniso=Aniso)

  # Build object
  dyn.load( paste0(TmbDir,"/",dynlib(Version)) )
  if(any(FieldConfig!=0)|any(VesselConfig!=0)){
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, random=Random, hessian=FALSE, map=Map, inner.method="newton")
  }else{
    Obj <- MakeADFun(data=TmbData, parameters=Parameters, hessian=FALSE, map=Map)
  }
  Obj$control <- list(trace=1, parscale=1, REPORT=1, reltol=1e-12, maxit=100)

  # Declare upper and lower bounds for parameter search
  Lower = rep(-50, length(Obj$par))
  Upper = rep( 50, length(Obj$par))
  names(Lower) = names(Upper) = names(Obj$par)
  Lower[grep("logsigmaV",names(Obj$par))] = log(0.01)
  Lower[grep("logsigmaVT",names(Obj$par))] = log(0.01)
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
  if( "Erho1" %in% names(Obj$par) ){
    Lower[grep("Erho1",names(Obj$par))] = -1
    Upper[grep("Erho1",names(Obj$par))] = 1
  }
  if( "Erho2" %in% names(Obj$par) ){
    Lower[grep("Erho2",names(Obj$par))] = -1
    Upper[grep("Erho2",names(Obj$par))] = 1
  }

  # Change convergence tolerance
  Obj$env$inner.control$step.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol] # Default : 1e-8  # Change in parameters limit inner optimization
  Obj$env$inner.control$tol10 <- c(1e-6,1e-8,1e-12)[ConvergeTol]  # Default : 1e-3     # Change in pen.like limit inner optimization
  Obj$env$inner.control$grad.tol <- c(1e-8,1e-12,1e-15)[ConvergeTol] # # Default : 1e-8  # Maximum gradient limit inner optimization

  # Return stuff
  Return = list("Obj"=Obj, "Upper"=Upper, "Lower"=Lower, "Parameters"=Parameters, "Map"=Map, "Random"=Random)
  return( Return )
}
