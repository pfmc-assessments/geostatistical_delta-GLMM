#include <TMB.hpp>

/** Precision matrix for the anisotropic case, eqn (20) in Lindgren et al. (2011) */    
namespace R_inla_generalized {
using namespace Eigen;
using namespace tmbutils;
using namespace R_inla;

template<class Type>
  SparseMatrix<Type> Q_spde_generalized(spde_t<Type> spde, Type kappa, int alpha=2){
  Type kappa_pow2 = kappa*kappa;
  Type kappa_pow4 = kappa_pow2*kappa_pow2;
  	
  if( alpha==1 ) return kappa_pow2*spde.M0 + spde.M1;
  if( alpha==2 ) return kappa_pow4*spde.M0 + Type(2.0)*kappa_pow2*spde.M1 + spde.M2;
}

template<class Type>
  SparseMatrix<Type> Q_spde_generalized(spde_aniso_t<Type> spde, Type kappa, matrix<Type> H, int alpha=2){

  int i;
  Type kappa_pow2 = kappa*kappa;
  Type kappa_pow4 = kappa_pow2*kappa_pow2;
  
  int n_s = spde.n_s;
  int n_tri = spde.n_tri;
  vector<Type> Tri_Area = spde.Tri_Area;
  matrix<Type> E0 = spde.E0;
  matrix<Type> E1 = spde.E1;
  matrix<Type> E2 = spde.E2;
  matrix<int> TV = spde.TV;
  SparseMatrix<Type> G0 = spde.G0;
  SparseMatrix<Type> G0_inv = spde.G0_inv;
	  	  
  //Type H_trace = H(0,0)+H(1,1);
  //Type H_det = H(0,0)*H(1,1)-H(0,1)*H(1,0);
  SparseMatrix<Type> G1_aniso(n_s,n_s); 
  SparseMatrix<Type> G2_aniso(n_s,n_s); 
  // Calculate adjugate of H
  matrix<Type> adj_H(2,2);
  adj_H(0,0) = H(1,1);
  adj_H(0,1) = -1 * H(0,1);
  adj_H(1,0) = -1 * H(1,0);
  adj_H(1,1) = H(0,0);
  // Calculate new SPDE matrices

  // Calculate G1 - pt. 1
  array<Type> Gtmp(n_tri,3,3);
  for(i=0; i<n_tri; i++){    
    // 1st line: E0(i,) %*% adjH %*% t(E0(i,)), etc.    
    Gtmp(i,0,0) = (E0(i,0)*(E0(i,0)*adj_H(0,0)+E0(i,1)*adj_H(1,0)) + E0(i,1)*(E0(i,0)*adj_H(0,1)+E0(i,1)*adj_H(1,1))) / (4*Tri_Area(i));  
    Gtmp(i,0,1) = (E1(i,0)*(E0(i,0)*adj_H(0,0)+E0(i,1)*adj_H(1,0)) + E1(i,1)*(E0(i,0)*adj_H(0,1)+E0(i,1)*adj_H(1,1))) / (4*Tri_Area(i));  
    Gtmp(i,0,2) = (E2(i,0)*(E0(i,0)*adj_H(0,0)+E0(i,1)*adj_H(1,0)) + E2(i,1)*(E0(i,0)*adj_H(0,1)+E0(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
    Gtmp(i,1,1) = (E1(i,0)*(E1(i,0)*adj_H(0,0)+E1(i,1)*adj_H(1,0)) + E1(i,1)*(E1(i,0)*adj_H(0,1)+E1(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
    Gtmp(i,1,2) = (E2(i,0)*(E1(i,0)*adj_H(0,0)+E1(i,1)*adj_H(1,0)) + E2(i,1)*(E1(i,0)*adj_H(0,1)+E1(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
    Gtmp(i,2,2) = (E2(i,0)*(E2(i,0)*adj_H(0,0)+E2(i,1)*adj_H(1,0)) + E2(i,1)*(E2(i,0)*adj_H(0,1)+E2(i,1)*adj_H(1,1))) / (4*Tri_Area(i));
  }
  // Calculate G1 - pt. 2
  for(i=0; i<n_tri; i++){
    G1_aniso.coeffRef(TV(i,1),TV(i,0)) = G1_aniso.coeffRef(TV(i,1),TV(i,0)) + (Gtmp(i,0,1));  
    G1_aniso.coeffRef(TV(i,0),TV(i,1)) = G1_aniso.coeffRef(TV(i,0),TV(i,1)) + (Gtmp(i,0,1));  
    G1_aniso.coeffRef(TV(i,2),TV(i,1)) = G1_aniso.coeffRef(TV(i,2),TV(i,1)) + (Gtmp(i,1,2));  
    G1_aniso.coeffRef(TV(i,1),TV(i,2)) = G1_aniso.coeffRef(TV(i,1),TV(i,2)) + (Gtmp(i,1,2));  
    G1_aniso.coeffRef(TV(i,2),TV(i,0)) = G1_aniso.coeffRef(TV(i,2),TV(i,0)) + (Gtmp(i,0,2));  
    G1_aniso.coeffRef(TV(i,0),TV(i,2)) = G1_aniso.coeffRef(TV(i,0),TV(i,2)) + (Gtmp(i,0,2));  
    G1_aniso.coeffRef(TV(i,0),TV(i,0)) = G1_aniso.coeffRef(TV(i,0),TV(i,0)) + (Gtmp(i,0,0));  
    G1_aniso.coeffRef(TV(i,1),TV(i,1)) = G1_aniso.coeffRef(TV(i,1),TV(i,1)) + (Gtmp(i,1,1));  
    G1_aniso.coeffRef(TV(i,2),TV(i,2)) = G1_aniso.coeffRef(TV(i,2),TV(i,2)) + (Gtmp(i,2,2));  
  }
  G2_aniso = G1_aniso * G0_inv * G1_aniso; 

  if( alpha==1 ) return kappa_pow2*G0 + G1_aniso;
  if( alpha==2 ) return kappa_pow4*G0 + Type(2.0)*kappa_pow2*G1_aniso + G2_aniso;
}
} // end namespace R_inla

// Function for detecting NAs
template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}

// dlnorm
template<class Type>
Type dlnorm(Type x, Type meanlog, Type sdlog, int give_log=0){
  //return 1/(sqrt(2*M_PI)*sd)*exp(-.5*pow((x-mean)/sd,2));
  Type logres = dnorm( log(x), meanlog, sdlog, true) - log(x);
  if(give_log) return logres; else return exp(logres);
}

// dlnorm
template<class Type>
Type dinvgauss(Type x, Type mean, Type shape, int give_log=0){
  Type logres = 0.5*log(shape) - 0.5*log(2*M_PI*pow(x,3)) - (shape * pow(x-mean,2) / (2*pow(mean,2)*x));
  if(give_log) return logres; else return exp(logres);
}

// dmixgamma
template<class Type>
Type dmixgamma(Type x, Type mean, Type cv1, Type mixprob, Type densratio, Type cv2, int give_log=0){
  //Type logres = log( 1e-250 + mixprob*dgamma(x, 1/pow(cv1,2), mean*pow(cv1,2), false) + (1-mixprob)*dgamma(x, 1/pow(cv2,2), mean*(1+densratio)*pow(cv2,2), false) );
  Type ll_1 = dgamma(x, 1/pow(cv1,2), mean*pow(cv1,2), true);
  Type ll_2 = dgamma(x, 1/pow(cv2,2), mean*(1+densratio)*pow(cv2,2), true);
  Type ll_offset = ll_1*invlogit(ll_1 - ll_2) + ll_2*invlogit(ll_2 - ll_1);
  Type logres = ll_offset + log( mixprob*exp(ll_1-ll_offset) + (1-mixprob)*exp(ll_2-ll_offset) );
  //Type logres = log( 1e-250 + mixprob*exp(ll_1) + (1-mixprob)*exp(ll_2) );
  if(give_log) return logres; else return exp(logres);
}

// dmixlnorm
template<class Type>
Type dmixlnorm(Type x, Type logmean, Type sdlog1, Type mixprob, Type densratio, Type sdlog2, int give_log=0){
  //Type logres = log( 1e-250 + mixprob*dlnorm(x,logmean-pow(sdlog1,2)/2, sdlog1, false) + (1-mixprob)*dlnorm(x,logmean+log(1+densratio)-pow(sdlog2,2)/2, sdlog2, false) );
  Type ll_1 = dlnorm(x,logmean-pow(sdlog1,2)/2, sdlog1, true);
  Type ll_2 = dlnorm(x,logmean+log(1+densratio)-pow(sdlog2,2)/2, sdlog2, true);
  Type ll_offset = ll_1*invlogit(ll_1 - ll_2) + ll_2*invlogit(ll_2 - ll_1);
  Type logres = ll_offset + log( mixprob*exp(ll_1-ll_offset) + (1-mixprob)*exp(ll_2-ll_offset) );
  //Type logres = log( 1e-250 + mixprob*exp(ll_1) + (1-mixprob)*exp(ll_2) );
  if(give_log) return logres; else return exp(logres);
}

// CMP distribution
template<class Type>
Type dCMP(Type x, Type mu, Type nu, int give_log=0, int iter_max=30, int break_point=10){
  // Explicit
  Type ln_S_1 = nu*mu - ((nu-1)/2)*log(mu) - ((nu-1)/2)*log(2*M_PI) - 0.5*log(nu);
  // Recursive
  vector<Type> S_i(iter_max);
  S_i(0) = 1;
  for(int i=1; i<iter_max; i++) S_i(i) = S_i(i-1) * pow( mu/Type(i), nu );
  Type ln_S_2 = log( sum(S_i) );
  // Blend (breakpoint:  mu=10)
  Type prop_1 = invlogit( (mu-break_point)*5 );
  //Type S_comb = prop_1*exp(ln_S_1) + (1-prop_1)*exp(ln_S_2);
  Type log_S_comb = prop_1*ln_S_1 + (1-prop_1)*ln_S_2;
  // Likelihood
  Type loglike = nu*x*log(mu) - nu*lgamma(x+1) - log_S_comb;
  // Return
  if(give_log) return loglike; else return exp(loglike);
}

// Space time
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace Eigen;
  using namespace density;
  using namespace R_inla_generalized;
  
  // Dimensions
  DATA_INTEGER(n_i);         // Number of observations (stacked across all years)
  DATA_INTEGER(n_s);         // Number of "strata" (i.e., vectices in SPDE mesh) 
  DATA_INTEGER(n_x);         // Number of real "strata" (i.e., k-means locations) 
  DATA_INTEGER(n_t);         // Number of years
  DATA_INTEGER(n_v);         // Number of vessels
  DATA_INTEGER(n_j);         // Number of static covariates
  DATA_INTEGER(n_p);         // Number of dynamic covariates
  DATA_INTEGER(n_k);          // Number of catchability variables
  DATA_INTEGER(n_l);         // Number of indices to post-process
  DATA_INTEGER(n_m);         // Number of range metrics to use (probably 2 for Eastings-Northings)

  // Config
  DATA_FACTOR( Options_vec );
  // Slot 0 -- Aniso: 0=No, 1=Yes
  // Slot 1 -- R2 interpretation: 0=R2 is positive_density, 1=R2 is density (including zeros)
  // Slot 2 -- AR1 on betas (year intercepts) to deal with missing years: 0=No, 1=Yes
  // Slot 3 -- smoothness alpha (1: alpha=1, 2: alpha=2)
  // Slot 4 -- Include penalty on area-abundance relationship: 0=No; 1=Yes (default=0)
  // Slot 5 -- Upper limit constant of integration calculation for Conway-Maxwell-Poisson density function
  // Slot 6 -- Breakpoint in CMP density function
  // Slot 7 -- Whether to use SPDE or 2D-AR1 hyper-distribution for spatial process: 0=SPDE; 1=2D-AR1
  DATA_FACTOR(FieldConfig);  // Input settings
  DATA_FACTOR(ObsModel);    // Observation model
  DATA_FACTOR(Options);    // Reporting options
  // Slot 0: Calculate SD for Index_xtl
  // Slot 1: Calculate SD for log(Index_xtl)
  // Slot 2: Calculate mean_relative_Z_tl
  // Slot 3: Calculate relative_evenness_t
  // Slot 4: Calculate mean_D_tl and effective_area_tl

  // Data vectors
  DATA_VECTOR(b_i);       	// Response (biomass) for each observation
  DATA_VECTOR(a_i);       	// Area swept for each observation (km^2)
  DATA_FACTOR(v_i)          // Vessel for each observation
  DATA_FACTOR(s_i)          // Station for each observation
  DATA_FACTOR(t_i)          // Year for each observation
  DATA_VECTOR(PredTF_i);          // tows/vessels for each observation (level of factor representing overdispersion)
  DATA_MATRIX(a_xl);		     // Area for each "real" stratum(km^2) in each stratum
  DATA_MATRIX(X_xj);		    // Covariate design matrix (strata x covariate)
  DATA_ARRAY(X_xtp);		    // Covariate design matrix (strata x covariate)
  DATA_MATRIX(Q_ik);        // Catchability matrix (observations x variable)
  DATA_MATRIX(Z_xm);        // Derived quantity matrix

  // SPDE objects
  DATA_STRUCT(spde,spde_t);
  
  // Aniso objects
  DATA_STRUCT(spde_aniso,spde_aniso_t);
  
  // Sparse matrices for precision matrix of 2D AR1 process
  // Q = M0*(1+rho^2)^2 + M1*(1+rho^2)*(-rho) + M2*rho^2
  DATA_SPARSE_MATRIX(M0);
  DATA_SPARSE_MATRIX(M1);
  DATA_SPARSE_MATRIX(M2);

  // Parameters
  PARAMETER_VECTOR(ln_H_input); // Anisotropy parameters
  PARAMETER_VECTOR(hyperparameters_z) // Miscellaneous hyperparameters, with interpretation that depends upon inputs
  //  -- presence/absence
  PARAMETER_VECTOR(beta1_t);  // Year effect
  PARAMETER_VECTOR(gamma1_j);        // Static covariate effect
  PARAMETER_MATRIX(gamma1_tp);       // Dynamic covariate effect
  PARAMETER_VECTOR(lambda1_k);       // Catchability coefficients
  PARAMETER(logetaE1);               // eta := kappa * tau
  PARAMETER(logetaO1);
  PARAMETER(logkappa1);
  PARAMETER(logsigmaV1);
  PARAMETER(logsigmaVT1);
  PARAMETER(Beta_mean1);  // mean-reversion for beta1_t
  PARAMETER(logsigmaB1);  // SD of beta1_t (default: not included in objective function)
  PARAMETER(Beta_rho1);  // AR1 for positive catch Epsilon component, Default=0
  PARAMETER(Epsilon_rho1);  // AR1 for presence/absence Epsilon component, Default=0
  //  -- random
  PARAMETER_VECTOR(nu1_v);
  PARAMETER_MATRIX(nu1_vt);
  // -- Gaussian random fields
  PARAMETER_VECTOR(Omegainput1_s);      // Expectation
  PARAMETER_ARRAY(Epsiloninput1_st);   // Annual variation
  //PARAMETER_MATRIX(Epsiloninput1_st);   // Annual variation
  //  -- positive catch rates
  PARAMETER_VECTOR(beta2_t);  // Year effect
  PARAMETER_VECTOR(gamma2_j);        // Covariate effect
  PARAMETER_MATRIX(gamma2_tp);       // Dynamic covariate effect
  PARAMETER_VECTOR(lambda2_k);       // Catchability coefficients
  PARAMETER(logetaE2);               // eta := kappa * tau
  PARAMETER(logetaO2);
  PARAMETER(logkappa2);
  PARAMETER(logsigmaV2);
  PARAMETER(logsigmaVT2);
  PARAMETER(Beta_mean2);  // mean-reversion for beta2_t
  PARAMETER(logsigmaB2);  // SD of beta2_t (default: not included in objective function)
  PARAMETER(Beta_rho2);  // AR1 for positive catch Epsilon component, Default=0
  PARAMETER(Epsilon_rho2);  // AR1 for positive catch Epsilon component, Default=0
  PARAMETER_VECTOR(logSigmaM);   // Slots: 0=mix1 CV, 1=prob-of-mix1, 2=
  // -- random
  PARAMETER_VECTOR(nu2_v);
  PARAMETER_MATRIX(nu2_vt);
  // -- Gaussian random fields  
  PARAMETER_VECTOR(Omegainput2_s);      // Expectation
  PARAMETER_ARRAY(Epsiloninput2_st);   // Annual variation
  //PARAMETER_MATRIX(Epsiloninput2_st);   // Annual variation

  // Indices -- i=Observation; j=Covariate; v=Vessel; t=Year; s=Stratum
  int i,j,v,t,s;             
  
  // Objective function
  vector<Type> jnll_comp(13);    // Slot 12=optional hyperparameters
  jnll_comp.setZero();
  Type jnll = 0;                
  
  // Derived parameters
  Type logtauE1 = logetaE1 - logkappa1;
  Type logtauO1 = logetaO1 - logkappa1;
  Type SigmaV1 = exp( logsigmaV1 );
  Type SigmaVT1 = exp( logsigmaVT1 );
  Type logtauE2 = logetaE2 - logkappa2;
  Type logtauO2 = logetaO2 - logkappa2;
  Type SigmaV2 = exp( logsigmaV2 );
  Type SigmaVT2 = exp( logsigmaVT2 );
  vector<Type> SigmaM(4);
  SigmaM(0) = exp(logSigmaM(0));
  SigmaM(1) = 1 / (1 + exp(-1 * logSigmaM(1)));
  SigmaM(2) = exp(logSigmaM(2));
  SigmaM(3) = exp(logSigmaM(3));

  // Derived that vary by settings
  // SEE: https://github.com/James-Thorson/TMB_experiments/tree/master/SPDE%20vs%202D_AR1
  Type SigmaE1, SigmaO1, Range_raw1, SigmaE2, SigmaO2, Range_raw2;
  if( Options_vec(7)==0 ){
    SigmaE1 = 1 / sqrt( 4*M_PI * exp(2*logtauE1) * exp(2*logkappa1) );
    SigmaO1 = 1 / sqrt( 4*M_PI * exp(2*logtauO1) * exp(2*logkappa1) );
    Range_raw1 = sqrt(8) / exp( logkappa1 );   // Range = approx. distance @ 10% correlation
    SigmaE2 = 1 / sqrt( 4*M_PI * exp(2*logtauE2) * exp(2*logkappa2) );
    SigmaO2 = 1 / sqrt( 4*M_PI * exp(2*logtauO2) * exp(2*logkappa2) );
    Range_raw2 = sqrt(8) / exp( logkappa2 );     // Range = approx. distance @ 10% correlation
  }
  if( Options_vec(7)==1 ){
    SigmaE1 = 1 / sqrt( (1-exp(logkappa1*2)) * exp(2*logtauE1) );
    SigmaO1 = 1 / sqrt( (1-exp(logkappa1*2)) * exp(2*logtauO1) );
    Range_raw1 = log(0.1) / logkappa1;   // Range = approx. distance @ 10% correlation
    SigmaE2 = 1 / sqrt( (1-exp(logkappa2*2)) * exp(2*logtauE2) );
    SigmaO2 = 1 / sqrt( (1-exp(logkappa2*2)) * exp(2*logtauO2) );
    Range_raw2 = log(0.1) / logkappa2;     // Range = approx. distance @ 10% correlation
  }

  // Anisotropy elements
  matrix<Type> H(2,2);
  H(0,0) = exp(ln_H_input(0));
  H(1,0) = ln_H_input(1);
  H(0,1) = ln_H_input(1);
  H(1,1) = (1+ln_H_input(1)*ln_H_input(1)) / exp(ln_H_input(0));

  // Derived fields
  vector<Type> Omega1_s(n_s);
  matrix<Type> Epsilon1_st(n_s,n_t);
  vector<Type> Omega2_s(n_s);
  matrix<Type> Epsilon2_st(n_s,n_t);
  for(s=0;s<n_s;s++){
    Omega1_s(s) = Omegainput1_s(s) / exp(logtauO1);
    Omega2_s(s) = Omegainput2_s(s) / exp(logtauO2);
    for(t=0;t<n_t;t++){
      Epsilon1_st(s,t) = Epsiloninput1_st(s,t) / exp(logtauE1);
      Epsilon2_st(s,t) = Epsiloninput2_st(s,t) / exp(logtauE2);
    }
  }
  
  // Random field probability                                                                                                                              
  Eigen::SparseMatrix<Type> Q1;
  Eigen::SparseMatrix<Type> Q2;
  if( Options_vec(7)==0 & Options_vec(0)==0 ){
    Q1 = Q_spde_generalized(spde, exp(logkappa1), Options_vec(3));
    Q2 = Q_spde_generalized(spde, exp(logkappa2), Options_vec(3));
  }
  if( Options_vec(7)==0 & Options_vec(0)==1 ){
    Q1 = Q_spde_generalized(spde_aniso, exp(logkappa1), H, Options_vec(3));
    Q2 = Q_spde_generalized(spde_aniso, exp(logkappa2), H, Options_vec(3));
  }
  if( Options_vec(7)==1 ){
    Q1 = M0*pow(1+exp(logkappa1*2),2) + M1*(1+exp(logkappa1*2))*(-exp(logkappa1)) + M2*exp(logkappa1*2);  // Same scale as SPDE method
    Q2 = M0*pow(1+exp(logkappa2*2),2) + M1*(1+exp(logkappa2*2))*(-exp(logkappa2)) + M2*exp(logkappa2*2);  // Same scale as SPDE method
  }
  GMRF_t<Type> gmrf1 = GMRF(Q1);
  GMRF_t<Type> gmrf2 = GMRF(Q2);
  if(FieldConfig(0)==1) jnll_comp(0) = gmrf1(Omegainput1_s);
  if(FieldConfig(1)==1){
    for(t=0;t<n_t;t++){
      if(t==0) jnll_comp(1) += gmrf1(Epsiloninput1_st.col(t));
      if(t>=1) jnll_comp(1) += gmrf1(Epsiloninput1_st.col(t)-Epsilon_rho1*Epsiloninput1_st.col(t-1));
    }
  }
  if(FieldConfig(2)==1) jnll_comp(2) = gmrf2(Omegainput2_s);
  if(FieldConfig(3)==1){
    for(t=0;t<n_t;t++){
      if(t==0) jnll_comp(3) += gmrf2(Epsiloninput2_st.col(t));
      if(t>=1) jnll_comp(3) += gmrf2(Epsiloninput2_st.col(t)-Epsilon_rho2*Epsiloninput2_st.col(t-1));
    }
  }

  // Random effect probabilities
  for(v=0;v<n_v;v++){
    jnll_comp(4) -= dnorm( nu1_v(v), Type(0.0), SigmaV1, true );
    jnll_comp(5) -= dnorm( nu2_v(v), Type(0.0), SigmaV2, true );
    for(t=0;t<n_t;t++){
      jnll_comp(6) -= dnorm( nu1_vt(v,t), Type(0.0), SigmaVT1, true );
      jnll_comp(7) -= dnorm( nu2_vt(v,t), Type(0.0), SigmaVT2, true );
    }
  }
  
  // Possible structure on betas
  if( Options_vec(2)!=0 ){
    for(t=1;t<n_t;t++){
      jnll_comp(8) -= dnorm( beta1_t(t), Beta_rho1*beta1_t(t-1) + Beta_mean1, exp(logsigmaB1), true );
      jnll_comp(9) -= dnorm( beta2_t(t), Beta_rho2*beta2_t(t-1) + Beta_mean2, exp(logsigmaB2), true );
    }
  }
  
  // Covariates
  vector<Type> eta1_x = X_xj * gamma1_j.matrix();
  vector<Type> zeta1_i = Q_ik * lambda1_k.matrix();
  vector<Type> eta2_x = X_xj * gamma2_j.matrix();
  vector<Type> zeta2_i = Q_ik * lambda2_k.matrix();
  matrix<Type> eta1_xt(n_x, n_t);
  matrix<Type> eta2_xt(n_x, n_t);
  eta1_xt.setZero();
  eta2_xt.setZero();
  for(int t=0; t<n_t; t++){
  for(int x=0; x<n_x; x++){
  for(int p=0; p<n_p; p++){
    eta1_xt(x,t) += gamma1_tp(t,p) * X_xtp(x,t,p);
    eta2_xt(x,t) += gamma2_tp(t,p) * X_xtp(x,t,p);
  }}}

  // Derived quantities
  vector<Type> var_i(n_i);
  Type var_y;
  // Linear predictor (pre-link) for presence/absence component
  vector<Type> P1_i(n_i);   
  // Response predictor (post-link)
  // ObsModel = 0:4 or 11:12: probability ("phi") that data is greater than zero
  // ObsModel = 5 (ZINB):  phi = 1-ZeroInflation_prob -> Pr[D=0] = NB(0|mu,var)*phi + (1-phi) -> Pr[D>0] = phi - NB(0|mu,var)*phi 
  vector<Type> R1_i(n_i);   
  vector<Type> LogProb1_i(n_i);
  // Linear predictor (pre-link) for positive component
  vector<Type> P2_i(n_i);   
  // Response predictor (post-link)
  // ObsModel = 0:3, 11:12:  expected value of data, given that data is greater than zero -> E[D] = mu*phi
  // ObsModel = 4 (ZANB):  expected value ("mu") of neg-bin PRIOR to truncating Pr[D=0] -> E[D] = mu/(1-NB(0|mu,var))*phi  ALSO  Pr[D] = NB(D|mu,var)/(1-NB(0|mu,var))*phi
  // ObsModel = 5 (ZINB):  expected value of data for non-zero-inflation component -> E[D] = mu*phi
  vector<Type> R2_i(n_i);   
  vector<Type> LogProb2_i(n_i);

  // Likelihood contribution from observations
  for (int i=0;i<n_i;i++){
    // Presence-absence prediction
    P1_i(i) =  beta1_t(t_i(i)) + Omega1_s(s_i(i)) + Epsilon1_st(s_i(i),t_i(i)) + eta1_x(s_i(i)) + eta1_xt(s_i(i),t_i(i)) + zeta1_i(i) + nu1_v(v_i(i)) + nu1_vt(v_i(i),t_i(i));
    R1_i(i) = invlogit( P1_i(i) ); 
    // Positive density prediction
    if( b_i(i)>0 | ObsModel(0)==5 | ObsModel(0)==6 ){    // 1e-500 causes overflow on laptop
      P2_i(i) =  beta2_t(t_i(i)) + Omega2_s(s_i(i)) + Epsilon2_st(s_i(i),t_i(i)) + eta2_x(s_i(i)) + eta2_xt(s_i(i),t_i(i)) + zeta2_i(i) + nu2_v(v_i(i)) + nu2_vt(v_i(i),t_i(i));
      R2_i(i) = exp( P2_i(i) );
    }else{
      P2_i(i) = 0;
      R2_i(i) = 0;
    }                                               
    // If Options_vec(1)==1, divide R2_i by R1_i so that R2_i represents R2_i*R1_i, i.e., local expected CPUE including zeros, i.e., local density*catchability
    if( Options_vec(1)==1 ){
      R2_i(i) = R2_i(i) / R1_i(i);
    }
    // Likelihood for models with continuous positive support 
    if(ObsModel(0)==0 | ObsModel(0)==1 | ObsModel(0)==2 | ObsModel(0)==11 | ObsModel(0)==12){ 
      // Presence-absence likelihood
      if( b_i(i) > 0 ){
        LogProb1_i(i) = log( R1_i(i) );
      }else{
        LogProb1_i(i) = log( 1-R1_i(i) );
      }
      // Positive density likelihood -- models with continuous positive support
      if( b_i(i) > 0 ){    // 1e-500 causes overflow on laptop
        if(ObsModel(0)==0) LogProb2_i(i) = dnorm(b_i(i), R2_i(i)*a_i(i), SigmaM(0), true);
        if(ObsModel(0)==1) LogProb2_i(i) = dlnorm(b_i(i), P2_i(i)+log(a_i(i))-pow(SigmaM(0),2)/2, SigmaM(0), true); // log-space
        if(ObsModel(0)==2) LogProb2_i(i) = dgamma(b_i(i), 1/pow(SigmaM(0),2), R2_i(i)*a_i(i)*pow(SigmaM(0),2), true); // shape = 1/CV^2, scale = mean*CV^2
        if(ObsModel(0)==11) LogProb2_i(i) = dmixlnorm(b_i(i), R2_i(i)*a_i(i), SigmaM(0), SigmaM(1), SigmaM(2), SigmaM(3), true); // log-space
        if(ObsModel(0)==12) LogProb2_i(i) = dmixgamma(b_i(i), R2_i(i)*a_i(i), SigmaM(0), SigmaM(1), SigmaM(2), SigmaM(3), true); // shape = 1/CV^2, scale = mean/CV^2
      }else{
        LogProb2_i(i) = 0;
      }
    }
    // Likelihood for models with discrete support 
    if(ObsModel(0)==4 | ObsModel(0)==5 | ObsModel(0)==6){
      var_i(i) = R2_i(i)*a_i(i)*(1.0+SigmaM(0)) + pow(R2_i(i)*a_i(i),2.0)*SigmaM(2);
      if(ObsModel(0)==4){
        if( b_i(i)==0 ){
          LogProb2_i(i) = log( 1-R1_i(i) ); 
        }else{
          LogProb2_i(i) = ( dnbinom2(b_i(i), R2_i(i)*a_i(i), var_i(i), true) - log(1-dnbinom2(Type(0.0), R2_i(i)*a_i(i), var_i(i), false)) + log(R1_i(i)) ); // Pr[X=x] = NB(X=x) / (1-NB(X=0)) * P(X=0)
        }
      }
      if(ObsModel(0)==5){
        if( b_i(i)==0 ){
          LogProb2_i(i) = log( (1-R1_i(i)) + dnbinom2(Type(0.0), R2_i(i)*a_i(i), var_i(i), false)*R1_i(i) ); //  Pr[X=0] = 1-phi + NB(X=0)*phi
        }else{
          LogProb2_i(i) = dnbinom2(b_i(i), R2_i(i)*a_i(i), var_i(i), true) + log(R1_i(i)); // Pr[X=x] = NB(X=x)*phi
        }
      }
      if(ObsModel(0)==6){
        LogProb2_i(i) = dCMP(b_i(i), R2_i(i)*a_i(i), exp(P1_i(i)), true, Options_vec(5));
      }
      LogProb1_i(i) = 0;
    }
  }

  // Predictive distribution -- ObsModel(4) isn't implemented (it had a bug previously)
  Type a_average = a_i.sum()/a_i.size();
  matrix<Type> P1_xt(n_x,n_t);
  matrix<Type> R1_xt(n_x,n_t);
  matrix<Type> P2_xt(n_x,n_t);
  matrix<Type> R2_xt(n_x,n_t);
  matrix<Type> D_xt(n_x,n_t);
  for(int t=0; t<n_t; t++){
  for(int x=0; x<n_x; x++){
    P1_xt(x,t) = beta1_t(t) + Omega1_s(x) + Epsilon1_st(x,t) + eta1_x(x) + eta1_xt(x,t);
    R1_xt(x,t) = invlogit( P1_xt(x,t) );     
    P2_xt(x,t) =  beta2_t(t) + Omega2_s(x) + Epsilon2_st(x,t) + eta2_x(x) + eta2_xt(x,t);
    if(ObsModel(0)==0 | ObsModel(0)==1 | ObsModel(0)==2 | ObsModel(0)==4 | ObsModel(0)==5) R2_xt(x,t) = exp( P2_xt(x,t) );
    if(ObsModel(0)==11 | ObsModel(0)==12) R2_xt(x,t) = SigmaM(1)*exp(P2_xt(x,t)) + (1-SigmaM(1))*exp(P2_xt(x,t))*(1+SigmaM(2) );
    // If Options_vec(1)==1, divide R2_i by R1_i so that R2_i represents R2_i*R1_i, i.e., local expected CPUE including zeros, i.e., local density*catchability
    if( Options_vec(1)==1 ){
      R2_xt(x,t) = R2_xt(x,t) / R1_xt(x,t);
    }
    // Expected value for predictive distribution in a grid cell
    if(ObsModel(0)==0 | ObsModel(0)==1 | ObsModel(0)==2 | ObsModel(0)==5 | ObsModel(0)==11 | ObsModel(0)==12) D_xt(x,t) = R1_xt(x,t) * R2_xt(x,t);
    if(ObsModel(0)==4){
      var_y = R2_xt(x,t)*a_average*(1.0+SigmaM(0)) + pow(R2_xt(x,t)*a_average,2.0)*SigmaM(2);
      D_xt(x,t) = R1_xt(x,t) / (1-dnbinom2(Type(0.0), R2_xt(x,t)*a_average, var_y, false)) * R2_xt(x,t);
    }
  }}
  
  // Calculate indices
  array<Type> Index_xtl(n_x,n_t,n_l);
  array<Type> Index_tl(n_t,n_l);
  array<Type> ln_Index_tl(n_t,n_l);
  Index_tl.setZero();
  for(int t=0; t<n_t; t++){
  for(int l=0; l<n_l; l++){
    for(int x=0; x<n_x; x++){
      Index_xtl(x,t,l) = D_xt(x,t) * a_xl(x,l) / 1000;  // Convert from kg to metric tonnes
      Index_tl(t,l) += Index_xtl(x,t,l); 
    }
  }}
  ln_Index_tl = log( Index_tl );

  // Calculate other derived summaries
  // Each is the weighted-average X_xl over polygons (x) with weights equal to abundance in each polygon and time (where abundance is from the first index)
  matrix<Type> mean_Z_tm(n_t, n_m);
  mean_Z_tm.setZero();
  int report_summary_TF = false;
  for(int t=0; t<n_t; t++){
  for(int m=0; m<n_m; m++){
    for(int x=0; x<n_x; x++){
      if( Z_xm(x,m)!=0 ){
        mean_Z_tm(t,m) += Z_xm(x,m) * Index_xtl(x,t,0)/Index_tl(t,0);
        report_summary_TF = true;
      }
    }
  }}
  if( report_summary_TF==true ){
    REPORT( mean_Z_tm );
    ADREPORT( mean_Z_tm );
  }
  if( Options(2)==1 ){
    matrix<Type> mean_relative_Z_tm(n_t,n_m);
    for(int t=0; t<n_t; t++){
    for(int m=0; m<n_m; m++){
      mean_relative_Z_tm(t,m) = mean_Z_tm(t,m) - mean_Z_tm(0,m);
    }}
    REPORT( mean_relative_Z_tm );
    ADREPORT( mean_relative_Z_tm );
  }

  // Calculate average density, weighted.mean( x=Abundance/Area, w=Abundance )
  // Doesn't require Z_xm, because it only depends upon Index_tl
  if( Options(4)==1 ){
    array<Type> mean_D_tl(n_t,n_l);
    array<Type> log_mean_D_tl(n_t,n_l);
    mean_D_tl.setZero();
    for(int t=0; t<n_t; t++){
    for(int l=0; l<n_l; l++){
      for(int x=0; x<n_x; x++){
        mean_D_tl(t,l) += D_xt(x,t) * Index_xtl(x,t,l)/Index_tl(t,l);
      }
    }}
    REPORT( mean_D_tl );
    ADREPORT( mean_D_tl );
    log_mean_D_tl = log( mean_D_tl );
    ADREPORT( log_mean_D_tl );

    // Calculate effective area = Index / average density
    array<Type> effective_area_tl(n_t,n_l);
    array<Type> log_effective_area_tl(n_t,n_l);
    effective_area_tl = Index_tl / (mean_D_tl/1000);  // Correct for different units of Index and density
    log_effective_area_tl = log( effective_area_tl );
    REPORT( effective_area_tl );
    ADREPORT( effective_area_tl );
    ADREPORT( log_effective_area_tl );
  }

  // Calculate the covariance kernal for density given covariates Z_xm and density Index_xtl for the first column of a_xl
  // Requires 2 dimensions for Z_xm to calculate cov_Z_tmm
  if( report_summary_TF==true & Z_xm.row(0).size()==2 ){

    // Calculate the kernel-area metric
    // DEPRECATED because this implementation causes problems with bias-correction (i.e., elements of area_Z_tmm = 0)
    if(false){
      array<Type> cov_Z_tmm(n_t, n_m, n_m);
      cov_Z_tmm.setZero();
      for(int t=0; t<n_t; t++){
      for(int m1=0; m1<n_m; m1++){
      for(int m2=0; m2<n_m; m2++){
        for(int x=0; x<n_x; x++){
          if(m1>=m2) cov_Z_tmm(t,m1,m2) += (Z_xm(x,m1)-mean_Z_tm(t,m1))*(Z_xm(x,m2)-mean_Z_tm(t,m2)) * Index_xtl(x,t,0)/Index_tl(t,0);
        }
      }}}
      REPORT( cov_Z_tmm );
      ADREPORT( cov_Z_tmm );

      // Calculate the area
      array<Type> area_Z_tmm(n_t, n_m, n_m);
      array<Type> log_area_Z_tmm(n_t, n_m, n_m);
      area_Z_tmm.setZero();
      for(int t=0; t<n_t; t++){
      for(int m1=0; m1<n_m; m1++){
      for(int m2=0; m2<n_m; m2++){
        // area of covariace matrix is determinant; det(Sigma) = ad - bc
        // see e.g., "volume of multivariate normal": https://onlinecourses.science.psu.edu/stat505/node/36
        if(m1>m2) area_Z_tmm(t,m1,m2) = pow( cov_Z_tmm(t,m1,m1)*cov_Z_tmm(t,m2,m2) - pow(cov_Z_tmm(t,m1,m2),2), 0.5 );
      }}}
      REPORT( area_Z_tmm );
      ADREPORT( area_Z_tmm );
      log_area_Z_tmm = log(area_Z_tmm);
      ADREPORT( log_area_Z_tmm );

      // Calculate the concentration (Index / SD) for density given covariates Z_xm
      array<Type> concentration_Z_tmm(n_t, n_m, n_m);
      array<Type> log_concentration_Z_tmm(n_t, n_m, n_m);
      concentration_Z_tmm.setZero();
      for(int t=0; t<n_t; t++){
      for(int m1=0; m1<n_m; m1++){
      for(int m2=0; m2<n_m; m2++){
        if(m1==m2) concentration_Z_tmm(t,m1,m1) = Index_tl(t,0) / pow( cov_Z_tmm(t,m1,m1), 0.5 );
        if(m1>m2) concentration_Z_tmm(t,m1,m2) = Index_tl(t,0) / area_Z_tmm(t,m1,m2);
      }}}
      REPORT( concentration_Z_tmm );
      ADREPORT( concentration_Z_tmm );
      log_concentration_Z_tmm = log(concentration_Z_tmm);
      ADREPORT( log_concentration_Z_tmm );

      // Testing hyperparameters
      if( Options_vec(4)==1 | Options_vec(4)==2 ){
        vector<Type> log_area_t_pred(n_t);
        for(int t=0; t<n_t; t++){
          log_area_t_pred(t) = hyperparameters_z(0) + hyperparameters_z(1)*ln_Index_tl(t,0);
          if( Options_vec(4)==1 ) jnll_comp(12) -= dnorm( log_area_Z_tmm(t,1,0), log_area_t_pred(t), exp(hyperparameters_z(2)), true );
        }
        REPORT( log_area_t_pred );
      }
    }
  }

  // Testing hyperparameters
  // DEPRECATED because I haven't found these useful
  if( false & Options_vec(4)==1 | Options_vec(4)==2 | Options(3)==1 ){
    // Calculate relative evenness = H'(t) / H'(max), where H' is Shannon diversity and H'(max) is maximum possible, except with weighting equal to area
    // Shannon diversity is generalized by the Kullback-Leibler divergence of the observed function P from a uniform function Q, Integral_x( P(x) * log(P(x)/Q(x)) * dx)
    // Integral_x( P(x)*dx ) = Integral_x( Q(x)*dx ) = 1;  units=m^-2
    // P(x), Q(x) are defined as density (abundance / area) rescaled to have integral of one
    // P(x) = Index_xtl(x,t,0) / a_xl(x,0) / Index_tl(t,0)
    // Q(x) = 1 / a_xl.col(0).sum()
    vector<Type> relative_evenness_t(n_t);
    vector<Type> log_relative_evenness_t(n_t);
    vector<Type> evenness_t(n_t);
    vector<Type> max_evenness_t(n_t);
    evenness_t.setZero();
    max_evenness_t.setZero();
    vector<Type> KLdivergence_t(n_t);
    KLdivergence_t.setZero();
    vector<Type> ln_relative_evenness_t_pred(n_t);
    // Loop through years
    for(int t=0; t<n_t; t++){
      for(int x=0; x<n_x; x++){
        if( a_xl(x,0)>0 ){
          evenness_t(t) -= (Index_xtl(x,t,0)/Index_tl(t,0)) * log( Index_xtl(x,t,0)/Index_tl(t,0) );   // Average density, weighted by area
          KLdivergence_t(t) += a_xl(x,0)/a_xl.col(0).sum() * (Index_xtl(x,t,0)/a_xl(x,0)/Index_tl(t,0)) * log( Index_xtl(x,t,0)/a_xl(x,0)/Index_tl(t,0) / (1/a_xl.col(0).sum()) );
        }
        max_evenness_t(t) -= (1/ float(n_x)) * log( 1/float(n_x) );
      }
      relative_evenness_t(t) = evenness_t(t) / max_evenness_t(t);             // Evenness relative to maximum
      // Likelihood component
      ln_relative_evenness_t_pred(t) = hyperparameters_z(0) + hyperparameters_z(1)*ln_Index_tl(t,0);
      if( Options_vec(4)==2 ) jnll_comp(12) -= dnorm( log(relative_evenness_t(t)), ln_relative_evenness_t_pred(t), exp(hyperparameters_z(2)), true );
    }
    log_relative_evenness_t = log( relative_evenness_t );
    REPORT( evenness_t );
    REPORT( max_evenness_t );
    REPORT( relative_evenness_t );
    REPORT( KLdivergence_t );
    REPORT( ln_relative_evenness_t_pred );
    ADREPORT( relative_evenness_t );
    ADREPORT( log_relative_evenness_t );
  }

  // Joint likelihood
  jnll_comp(10) = -1*(LogProb1_i*(Type(1.0)-PredTF_i)).sum();
  jnll_comp(11) = -1*(LogProb2_i*(Type(1.0)-PredTF_i)).sum();
  jnll = jnll_comp.sum();
  Type pred_jnll = -1 * ( LogProb1_i*PredTF_i + LogProb2_i*PredTF_i ).sum();
  REPORT( pred_jnll );

  // Diagnostic output
  REPORT( P1_i );
  REPORT( P2_i );
  REPORT( R1_i );
  REPORT( R2_i );
  REPORT( P1_xt );
  REPORT( P2_xt );
  REPORT( var_i );
  REPORT( LogProb1_i );
  REPORT( LogProb2_i );
  REPORT( a_average );
  REPORT( eta1_x );
  REPORT( eta2_x );
  REPORT( eta1_xt );
  REPORT( eta2_xt );
  REPORT( zeta1_i );
  REPORT( zeta2_i );
  
  REPORT( SigmaE1 );
  REPORT( SigmaO1 );
  REPORT( SigmaE2 );
  REPORT( SigmaO2 );
  REPORT( SigmaM );
  REPORT( SigmaV1 );
  REPORT( SigmaV2 );
  REPORT( SigmaVT1 );
  REPORT( SigmaVT2 );
  REPORT( Index_tl );
  REPORT( D_xt );
  REPORT( R1_xt );
  REPORT( R2_xt );
  REPORT( Index_xtl );
  REPORT( Omega1_s );
  REPORT( Omega2_s );
  REPORT( Epsilon1_st );
  REPORT( Epsilon2_st );
  REPORT( H );
  REPORT( Range_raw1 );
  REPORT( Range_raw2 );
  REPORT( beta1_t );
  REPORT( beta2_t );
  REPORT( jnll_comp );
  REPORT( jnll );
  REPORT( hyperparameters_z );

  ADREPORT( Range_raw1 );
  ADREPORT( Range_raw2 );
  ADREPORT( Index_tl );
  ADREPORT( ln_Index_tl);
  ADREPORT( SigmaE1 );
  ADREPORT( SigmaO1 );
  ADREPORT( SigmaE2 );
  ADREPORT( SigmaO2 );
  ADREPORT( SigmaM );
  ADREPORT( SigmaV1 );
  ADREPORT( SigmaV2 );
  ADREPORT( SigmaVT1 );
  ADREPORT( SigmaVT2 );

  // Additional miscellaneous outputs
  if( Options(0)==1 ){
    ADREPORT( Index_xtl );
  }
  if( Options(1)==1 ){
    ADREPORT( log(Index_xtl) );
  }

  return jnll;
  
}
