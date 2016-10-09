
#' Build data input for geostatistical delta-GLMM
#'
#' \code{Data_Fn} builds a tagged list of data inputs used by TMB for running the model
#'
#' @param Version a version number (see example for current default).
#' @param FieldConfig a vector of format c("Omega1"=0, "Epsilon1"=10, "Omega2"="AR1", "Epsilon2"=10), where Omega refers to spatial variation, Epsilon refers to spatio-temporal variation, Omega1 refers to variation in encounter probability, and Omega2 refers to variation in positive catch rates, where 0 is off, "AR1" is an AR1 process, and >0 is the number of elements in a factor-analysis covariance
#' @param ObsModel an optional integer that specifies the distribution for positive catch rates (Default=2, i.e., Gamma)
#' \describe{
#'   \item{ObsModel=0}{Normal}
#'   \item{ObsModel=1}{Lognormal}
#'   \item{ObsModel=2}{Gamma}
#'   \item{ObsModel=3}{A tagged list of parameter starting values used when building Obj, which can be extracted, modified, and then put back into \code{Build_TMB_Fn} to define different starting values}
#'   \item{ObsModel=11}{Mixture lognormal}
#'   \item{ObsModel=12}{Mixture gamma}
#'   \item{ObsModel=4}{Zero-adjusted negative binomial}
#'   \item{ObsModel=5}{Zero-inflated negativee binomial}
#'   \item{ObsModel=6}{Conway-Maxwell-Poisson distribution}
#' }
#' @param b_i Sampled biomass for each observation i
#' @param a_i Sampled area for each observation i
#' @param s_i Spatial knot (e.g., grid cell) for each observation i
#' @param t_i Time interval (e.g., year) for each observation i
#' @param a_xl Area associated with each knot
#' @param MeshList, tagged list representing location information for the SPDE mesh hyperdistribution
#' @param GridList, tagged list representing location information for the 2D AR1 grid hyperdistribution
#' @param Method, character (either "Mesh" or "Grid") specifying hyperdistribution (Default="Mesh")
#' @param v_i OPTIONAL, sampling category (e.g., vessel or tow) associated with overdispersed variation for each observation i
#' @param PredTF_i OPTIONAL, whether each observation i is included in the likelihood (PredTF_i[i]=0) or in the predictive probability (PredTF_i[i]=1)
#' @param X_xj OPTIONAL, matrix of static density covariates (e.g., measured variables affecting density, as used when interpolating density for calculating an index of abundance)
#' @param X_xtp OPTIONAL, array of dynamic (varying among time intervals) density covariates
#' @param Q_ik OPTIONAL, matrix of catchability covariates (e.g., measured variables affecting catch rates but not caused by variation in species density) for each observation i
#' @param Aniso OPTIONAL whether to assume isotropy (Aniso=0) or geometric anisotropy (Aniso=1)
#' @param RhoConfig OPTIONAL, vector of form c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0) specifying whether either intercepts (Beta1 and Beta2) or spatio-temporal variation (Epsilon1 and Epsilon2) is structured among time intervals
#' @param Options OPTIONAL, a vector of form c('SD_site_density'=0,'SD_site_logdensity'=0,'Calculate_Range'=0,'Calculate_evenness'=0,'Calculate_effective_area'=0), where Calculate_Range=1 turns on calculation of center of gravity, and Calculate_effective_area=1 turns on calculation of effective area occupied
#' @param CheckForErrors OPTIONAL, whether to check for errors in input (NOTE: when CheckForErrors=TRUE, the function will throw an error if it detects a problem with inputs.  However, failing to throw an error is no guaruntee that the inputs are all correct)

#' @return Tagged list containing inputs to function Build_TMB_Fn()

#' @export
Data_Fn <-
function( Version, FieldConfig, ObsModel=2, b_i, a_i, s_i, t_i, a_xl, MeshList, GridList=NULL, Method="Mesh", v_i=rep(0,length(b_i)), PredTF_i=rep(0,length(b_i)), X_xj=NULL, X_xtp=NULL, Q_ik=NULL, Aniso=1, RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0), Options=c('SD_site_density'=0,'SD_site_logdensity'=0,'Calculate_Range'=0,'Calculate_evenness'=0,'Calculate_effective_area'=0), CheckForErrors=TRUE, Alpha=2 ){

  # Determine dimensions
  n_t = max(t_i) - min(t_i) + 1
  n_v = max(v_i) + 1
  n_i = length(b_i)
  n_x = nrow(a_xl)
  n_l = ncol(a_xl)

  # Covariates and defaults
  if( is.null(X_xj) ) X_xj = matrix(0, nrow=n_x, ncol=1)
  if( is.null(X_xtp) ) X_xtp = array(0, dim=c(n_x,n_t,1))
  if( is.null(Q_ik) ) Q_ik = matrix(0, nrow=n_i, ncol=1)
  n_j = ncol(X_xj)
  n_p = dim(X_xtp)[3]
  n_k = ncol(Q_ik)

  # by default, add nothing as Z_xl
  if( Options['Calculate_Range']==FALSE ){
    Z_xm = matrix(0, nrow=nrow(a_xl), ncol=ncol(a_xl) ) # Size so that it works for Version 3g-3j
  }
  if(Options['Calculate_Range']==TRUE ){
    Z_xm = MeshList$loc_x
    message( "Calculating range shift for stratum #1:",colnames(a_xl[1]))
  }

  # Check for bad data entry
  if( CheckForErrors==TRUE ){
    if( !is.matrix(a_xl) | !is.matrix(X_xj) | !is.matrix(Q_ik) ) stop("a_xl, X_xj, and Q_ik should be matrices")
    if( (max(s_i)-1)>n_x | min(s_i)<0 ) stop("s_i exceeds bounds in MeshList")
    if( any(a_i<=0) ) stop("a_i must be greater than zero for all observations, and at least one value of a_i is not")
    # Warnings about all positive or zero
    Prop_nonzero = tapply( b_i, INDEX=t_i, FUN=function(vec){mean(vec>0)} )
    if( any(Prop_nonzero==0|Prop_nonzero==1) & any(FieldConfig[1:2]==1) ){
      print( Prop_nonzero )
      stop("Some years have either all or no encounters")
    }
  }

  # Check for bad data entry
  if( CheckForErrors==TRUE ){
    if( length(b_i)!=n_i | length(a_i)!=n_i | length(v_i)!=n_i | length(s_i)!=n_i | length(t_i)!=n_i ) stop("b_i, a_i, v_i, s_i, or t_i doesn't have length n_i")
    if( nrow(a_xl)!=n_x | ncol(a_xl)!=n_l ) stop("a_xl has wrong dimensions")
    if( nrow(X_xj)!=n_x | ncol(X_xj)!=n_j ) stop("X_xj has wrong dimensions")
    if( nrow(Q_ik)!=n_i | ncol(Q_ik)!=n_k ) stop("Q_ik has wrong dimensions")
    if( length(unique(v_i))!=n_v ) stop("Please renumber v_i consecutively from 0 to final vessel minus 1")
  }

  # switch defaults if necessary
  if( Method=="Grid" & is.null(GridList)) stop("To use 2D AR1 grid distribution, please provide GridList")
  if( Method=="Grid" ){
    Aniso = 0
    message("Using isotropic 2D AR1 hyperdistribution, so switching to Aniso=0")
  }

  # Output tagged list
  Options_vec = c("Aniso"=Aniso, "R2_interpretation"=0, "Rho_betaTF"=ifelse(RhoConfig[["Beta1"]]|RhoConfig[["Beta2"]],1,0), "Alpha"=Alpha, "AreaAbundanceCurveTF"=0, "CMP_xmax"=30, "CMP_breakpoint"=10, "Method"=switch(Method,"Mesh"=0,"Grid"=1) )
  if(Version=="geo_index_v3a"){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_x"=a_xl[,1], "X_xj"=X_xj, "Q_ik"=Q_ik, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0_inv"=INLA::inla.as.dgTMatrix(solve(MeshList$spde$param.inla$M0)), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version=="geo_index_v3b"){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0_inv"=INLA::inla.as.dgTMatrix(solve(MeshList$spde$param.inla$M0)), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version%in%c("geo_index_v3d","geo_index_v3c")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "spde"=list(), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version%in%c("geo_index_v3e")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Options_vec"=c(Aniso,0), "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "spde"=list(), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version%in%c("geo_index_v3g","geo_index_v3f")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Options_vec"=Options_vec, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "Z_xl"=Z_xm, "spde"=list(), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version%in%c("geo_index_v3i","geo_index_v3h")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Options_vec"=Options_vec, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "Z_xl"=Z_xm, "spde"=list(), "spde_aniso"=list(), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version%in%c("geo_index_v3j")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Options_vec"=Options_vec, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "Z_xl"=Z_xm, "spde"=list(), "spde_aniso"=list() )
  }
  if(Version%in%c("geo_index_v3k")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "n_m"=ncol(Z_xm), "Options_vec"=Options_vec, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "Z_xm"=Z_xm, "spde"=list(), "spde_aniso"=list() )
  }
  if(Version%in%c("geo_index_v3m","geo_index_v3l")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_p"=n_p, "n_k"=n_k, "n_l"=n_l, "n_m"=ncol(Z_xm), "Options_vec"=Options_vec, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "a_xl"=a_xl, "X_xj"=X_xj, "X_xtp"=X_xtp, "Q_ik"=Q_ik, "Z_xm"=Z_xm, "spde"=list(), "spde_aniso"=list() )
  }
  if(Version%in%c("geo_index_v3n")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_p"=n_p, "n_k"=n_k, "n_l"=n_l, "n_m"=ncol(Z_xm), "Options_vec"=Options_vec, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "PredTF_i"=PredTF_i, "a_xl"=a_xl, "X_xj"=X_xj, "X_xtp"=X_xtp, "Q_ik"=Q_ik, "Z_xm"=Z_xm, "spde"=list(), "spde_aniso"=list() )
  }
  if(Version%in%c("geo_index_v4b","geo_index_v4a")){
    Return = list( "n_i"=n_i, "n_s"=c(MeshList$spde$n.spde,n_x)[Options_vec['Method']+1], "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_p"=n_p, "n_k"=n_k, "n_l"=n_l, "n_m"=ncol(Z_xm), "Options_vec"=Options_vec, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i-min(t_i), "PredTF_i"=PredTF_i, "a_xl"=a_xl, "X_xj"=X_xj, "X_xtp"=X_xtp, "Q_ik"=Q_ik, "Z_xm"=Z_xm, "spde"=list(), "spde_aniso"=list(), "M0"=GridList$M0, "M1"=GridList$M1, "M2"=GridList$M2 )
  }
  if( "spde" %in% names(Return) ) Return[['spde']] = INLA::inla.spde2.matern(MeshList$mesh)$param.inla[c("M0","M1","M2")]
  if( "spde_aniso" %in% names(Return) ) Return[['spde_aniso']] = list("n_s"=MeshList$spde$n.spde, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0"=MeshList$spde$param.inla$M0, "G0_inv"=INLA::inla.as.dgTMatrix(solve(MeshList$spde$param.inla$M0)) )

  # Feed in a default M0/M1/M2 in the event that the user doesn't specify values
  if( is.null(GridList) & "spde"%in%names(Return) & "M0"%in%names(Return) ) Return[c("M0","M1","M2")] = Return$spde[c("M0","M1","M2")]

  # Check for NAs
  if( CheckForErrors==TRUE ){
    if( any(sapply(Return, FUN=function(Array){any(is.na(Array))})==TRUE) ) stop("Please find and eliminate the NA from your inputs") 
  }

  # Return
  return( Return )
}
