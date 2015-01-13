#include <TMB.hpp>

// dlnorm
template<class Type>
Type dlnorm(Type x, Type meanlog, Type sdlog, int give_log=0){
  Type logres;
  logres = dnorm( log(x), meanlog, sdlog, true) - log(x);
  //return 1/(sqrt(2*M_PI)*sd)*exp(-.5*pow((x-mean)/sd,2));
  if(give_log)return logres; else return exp(logres);
}

// Space time 
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Dimensions
  DATA_INTEGER(n_i);         // Number of observations (stacked across all years)
  DATA_INTEGER(n_s);         // Number of "strata" (i.e., vectices in SPDE mesh) 
  DATA_INTEGER(n_x);         // Number of real "strata" (i.e., k-means locations) 
  DATA_INTEGER(n_t);         // Number of years
  DATA_INTEGER(n_v);         // Number of vessels
  DATA_INTEGER(n_j);         // Number of covariates
  
  // Config
  DATA_INTEGER(Aniso); // 0: No; 1:Yes
  DATA_FACTOR(FieldConfig);  // Input settings
  DATA_FACTOR(ObsModel);    // Observation model
  DATA_FACTOR(Options);    // Reporting options

  // Data vectors
  DATA_VECTOR(b_i);       	// Response (biomass) for each observation
  DATA_VECTOR(a_i);       	// Area swept for each observation (km^2)
  DATA_FACTOR(v_i)          // Vessel for each observation
  DATA_FACTOR(s_i)          // Station for each observation
  DATA_FACTOR(t_i)          // Year for each observation
  DATA_VECTOR(a_x);		     // Area for each "real" stratum(km^2)
  DATA_MATRIX(X_xj);		    // Covariate design matrix (observation x covariate)

  // Aniso objects
  DATA_INTEGER(n_tri);      //  Number of triangles
  DATA_VECTOR(Tri_Area);
  DATA_MATRIX(E0);
  DATA_MATRIX(E1);
  DATA_MATRIX(E2);
  DATA_FACTOR(TV);        //  This already includes the -1 for indexing in C
  DATA_SPARSE_MATRIX(G0_inv);

  // SPDE objects
  DATA_SPARSE_MATRIX(G0);
  DATA_SPARSE_MATRIX(G1);
  DATA_SPARSE_MATRIX(G2);

  // Parameters 
  PARAMETER_VECTOR(ln_H_input); // Anisotropy parameters
  //  -- presence/absence
  PARAMETER_VECTOR(beta1_t);  // Year effect
  PARAMETER_VECTOR(gamma1_j);        // Covariate effect
  PARAMETER(logetaE1);      
  PARAMETER(logetaO1);
  PARAMETER(logkappa1);
  PARAMETER(logsigmaV1);
  PARAMETER(logsigmaVT1);
  //  -- random
  PARAMETER_VECTOR(nu1_v);
  PARAMETER_MATRIX(nu1_vt);
  // -- Gaussian random fields
  PARAMETER_VECTOR(Omegainput1_s);      // Expectation
  PARAMETER_MATRIX(Epsiloninput1_st);   // Annual variation
  //  -- positive catch rates
  PARAMETER_VECTOR(beta2_t);  // Year effect
  PARAMETER_VECTOR(gamma2_j);        // Covariate effect
  PARAMETER(logetaE2);      
  PARAMETER(logetaO2);
  PARAMETER(logkappa2);
  PARAMETER(logsigmaV2);
  PARAMETER(logsigmaVT2);
  PARAMETER_VECTOR(logSigmaM);
  // -- random
  PARAMETER_VECTOR(nu2_v);
  PARAMETER_MATRIX(nu2_vt);
  // -- Gaussian random fields  
  PARAMETER_VECTOR(Omegainput2_s);      // Expectation
  PARAMETER_MATRIX(Epsiloninput2_st);   // Annual variation

  using namespace density;
  int i,j,v,t,s;             // i=Observation; j=Covariate; v=Vessel; t=Year; s=Stratum
  Type NLL = 0;                // Objective function
  
  // Derived parameters
  Type pi = 3.141592;
  Type logtauE1 = logetaE1 - logkappa1;
  Type logtauO1 = logetaO1 - logkappa1;
  Type kappa1_pow2 = exp(2.0*logkappa1);
  Type kappa1_pow4 = kappa1_pow2*kappa1_pow2;
  Type SigmaE1 = 1 / sqrt(4*pi*exp(2*logtauE1)*exp(2*logkappa1));
  Type SigmaO1 = 1 / sqrt(4*pi*exp(2*logtauO1)*exp(2*logkappa1));
  Type Range_raw1 = sqrt(8) / exp( logkappa1 );
  Type SigmaV1 = exp( logsigmaV1 );
  Type SigmaVT1 = exp( logsigmaVT1 );

  Type logtauE2 = logetaE2 - logkappa2;
  Type logtauO2 = logetaO2 - logkappa2;
  Type kappa2_pow2 = exp(2.0*logkappa2);
  Type kappa2_pow4 = kappa2_pow2*kappa2_pow2;
  Type SigmaE2 = 1 / sqrt(4*pi*exp(2*logtauE2)*exp(2*logkappa2));
  Type SigmaO2 = 1 / sqrt(4*pi*exp(2*logtauO2)*exp(2*logkappa2));
  Type Range_raw2 = sqrt(8) / exp( logkappa2 );
  Type SigmaV2 = exp( logsigmaV2 );
  Type SigmaVT2 = exp( logsigmaVT2 );
  vector<Type> SigmaM(4);
  SigmaM(0) = exp(logSigmaM(0));
  SigmaM(1) = 1 / (1 + exp(-1 * logSigmaM(1)));
  SigmaM(2) = exp(logSigmaM(2));
  SigmaM(3) = exp(logSigmaM(3));
  
  // Anisotropy elements
  matrix<Type> H(2,2);
  H(0,0) = exp(ln_H_input(0));
  H(1,0) = ln_H_input(1);
  H(0,1) = ln_H_input(1);
  H(1,1) = (1+ln_H_input(1)*ln_H_input(1)) / exp(ln_H_input(0));
  Type H_trace = H(0,0)+H(1,1);
  Type H_det = H(0,0)*H(1,1)-H(0,1)*H(1,0);
  //Type Range_major1 = Range_raw1*(H_trace/2+sqrt(H_trace*H_trace/4-H_det));
  //Type Range_minor1 = Range_raw1*(H_trace/2-sqrt(H_trace*H_trace/4-H_det));
  //Type Range_major2 = Range_raw2*(H_trace/2+sqrt(H_trace*H_trace/4-H_det));
  //Type Range_minor2 = Range_raw2*(H_trace/2-sqrt(H_trace*H_trace/4-H_det));
  Eigen::SparseMatrix<Type> G1_aniso(n_s,n_s); 
  Eigen::SparseMatrix<Type> G2_aniso(n_s,n_s); 
  // Calculate adjugate of H
  matrix<Type> adj_H(2,2);
  adj_H(0,0) = H(1,1);
  adj_H(0,1) = -1 * H(0,1);
  adj_H(1,0) = -1 * H(1,0);
  adj_H(1,1) = H(0,0);
  // Calculate new SPDE matrices
  if(Aniso==1){
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
      int i0 = i;
      int i1 = i + n_tri; 
      int i2 = i + 2*n_tri; 
      G1_aniso.coeffRef(TV(i1),TV(i0)) = G1_aniso.coeffRef(TV(i1),TV(i0)) + (Gtmp(i,0,1));  
      G1_aniso.coeffRef(TV(i0),TV(i1)) = G1_aniso.coeffRef(TV(i0),TV(i1)) + (Gtmp(i,0,1));  
      G1_aniso.coeffRef(TV(i2),TV(i1)) = G1_aniso.coeffRef(TV(i2),TV(i1)) + (Gtmp(i,1,2));  
      G1_aniso.coeffRef(TV(i1),TV(i2)) = G1_aniso.coeffRef(TV(i1),TV(i2)) + (Gtmp(i,1,2));  
      G1_aniso.coeffRef(TV(i2),TV(i0)) = G1_aniso.coeffRef(TV(i2),TV(i0)) + (Gtmp(i,0,2));  
      G1_aniso.coeffRef(TV(i0),TV(i2)) = G1_aniso.coeffRef(TV(i0),TV(i2)) + (Gtmp(i,0,2));  
      G1_aniso.coeffRef(TV(i0),TV(i0)) = G1_aniso.coeffRef(TV(i0),TV(i0)) + (Gtmp(i,0,0));  
      G1_aniso.coeffRef(TV(i1),TV(i1)) = G1_aniso.coeffRef(TV(i1),TV(i1)) + (Gtmp(i,1,1));  
      G1_aniso.coeffRef(TV(i2),TV(i2)) = G1_aniso.coeffRef(TV(i2),TV(i2)) + (Gtmp(i,2,2));  
    }
    G2_aniso = G1_aniso * G0_inv * G1_aniso; 
  }
  
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
  if(Aniso==0){
    Q1 = kappa1_pow4*G0 + Type(2.0)*kappa1_pow2*G1 + G2;
    Q2 = kappa1_pow4*G0 + Type(2.0)*kappa1_pow2*G1 + G2;
  }
  if(Aniso==1){
    Q1 = kappa1_pow4*G0 + Type(2.0)*kappa1_pow2*G1_aniso + G2_aniso;
    Q2 = kappa2_pow4*G0 + Type(2.0)*kappa2_pow2*G1_aniso + G2_aniso;
  }
  GMRF_t<Type> Tmp1 = GMRF(Q1);
  GMRF_t<Type> Tmp2 = GMRF(Q2);
  for(t=0;t<n_t;t++){
    if(FieldConfig(1)==1) NLL += Tmp1(Epsiloninput1_st.col(t));
    if(FieldConfig(3)==1) NLL += Tmp2(Epsiloninput2_st.col(t));
  }
  if(FieldConfig(0)==1) NLL += Tmp1(Omegainput1_s);
  if(FieldConfig(2)==1) NLL += Tmp2(Omegainput2_s);

  // Random effect probabilities
  for(v=0;v<n_v;v++){
    NLL -= dnorm( nu1_v(v), Type(0.0), SigmaV1, 1 );
    NLL -= dnorm( nu2_v(v), Type(0.0), SigmaV2, 1 );
    for(t=0;t<n_t;t++){
      NLL -= dnorm( nu1_vt(v,t), Type(0.0), SigmaVT1, 1 );
      NLL -= dnorm( nu2_vt(v,t), Type(0.0), SigmaVT2, 1 );
    }
  }
  
  // Likelihood contribution from observations
  Type var_y;
  // Linear predictor (pre-link) for presence/absence component
  vector<Type> P1_i(n_i);   
  // Response predictor (post-link)
  // ObsModel = 0:4 or 11:12: probability ("phi") that data is greater than zero
  // ObsModel = 5 (ZINB):  phi = 1-ZeroInflation_prob -> Pr[D=0] = NB(0|mu,var)*phi + (1-phi) -> Pr[D>0] = phi - NB(0|mu,var)*phi 
  vector<Type> R1_i(n_i);   
  vector<Type> LogProb1_i(n_i);
  vector<Type> eta1_x(n_i);
  eta1_x = X_xj * gamma1_j.matrix();
  // Linear predictor (pre-link) for positive component
  vector<Type> P2_i(n_i);   
  // Response predictor (post-link)
  // ObsModel = 0:3, 11:12:  expected value of data, given that data is greater than zero -> E[D] = mu*phi
  // ObsModel = 4 (ZANB):  expected value ("mu") of neg-bin PRIOR to truncating Pr[D=0] -> E[D] = mu/(1-NB(0|mu,var))*phi  ALSO  Pr[D] = NB(D|mu,var)/(1-NB(0|mu,var))*phi
  // ObsModel = 5 (ZINB):  expected value of data for non-zero-inflation component -> E[D] = mu*phi
  vector<Type> R2_i(n_i);   
  vector<Type> LogProb2_i(n_i);
  vector<Type> eta2_x(n_i);
  eta2_x = X_xj * gamma2_j.matrix();
  for (int i=0;i<n_i;i++){
    // Presence-absence prediction
    P1_i(i) =  beta1_t(t_i(i)) + Omega1_s(s_i(i)) + Epsilon1_st(s_i(i),t_i(i)) + eta1_x(s_i(i)) + nu1_v(v_i(i)) + nu1_vt(v_i(i),t_i(i));
    R1_i(i) = 1 / (1 + exp(-P1_i(i)));
    // Positive density prediction
    if( b_i(i)>0 | ObsModel(0)==5 ){    // 1e-500 causes overflow on laptop
      P2_i(i) =  beta2_t(t_i(i)) + Omega2_s(s_i(i)) + Epsilon2_st(s_i(i),t_i(i)) + eta2_x(s_i(i)) + nu2_v(v_i(i)) + nu2_vt(v_i(i),t_i(i));
      R2_i(i) = exp( P2_i(i) );
    }else{
      P2_i(i) = 0;
      R2_i(i) = 0;
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
        if(ObsModel(0)==11) LogProb2_i(i) = log( 1e-250 + SigmaM(1)*dlnorm(b_i(i),P2_i(i)+log(a_i(i))-pow(SigmaM(0),2)/2, SigmaM(0), false) + (1-SigmaM(1))*dlnorm(b_i(i),P2_i(i)+log(a_i(i))+log(1+SigmaM(2))-pow(SigmaM(3),2)/2, SigmaM(3), false) ); // log-space
        if(ObsModel(0)==12) LogProb2_i(i) = log( 1e-250 + SigmaM(1)*dgamma(b_i(i), 1/pow(SigmaM(0),2), R2_i(i)*a_i(i)*pow(SigmaM(0),2), false) + (1-SigmaM(1))*dgamma(b_i(i), 1/pow(SigmaM(3),2), R2_i(i)*a_i(i)*(1+SigmaM(2))*pow(SigmaM(3),2), false) ); // shape = 1/CV^2, scale = mean/CV^2
      }else{
        LogProb2_i(i) = 0;
      }
    }
    // Likelihood for models with discrete support 
    if(ObsModel(0)==4 | ObsModel(0)==5){ 
      var_y = R2_i(i)*a_i(i)*(1.0+SigmaM(0)) + pow(R2_i(i)*a_i(i),2.0)*SigmaM(2);
      if(ObsModel(0)==4){
        if( b_i(i)==0 ){    
          LogProb2_i(i) = log( 1-R1_i(i) ); 
        }else{
          LogProb2_i(i) = ( dnbinom2(b_i(i), R2_i(i)*a_i(i), var_y, true) - log(1-dnbinom2(Type(0.0), R2_i(i)*a_i(i), var_y, false)) + log(R1_i(i)) ); // Pr[X=x] = NB(X=x) / (1-NB(X=0)) * P(X=0)
        }
      }
      if(ObsModel(0)==5){
        if( b_i(i)==0 ){    
          LogProb2_i(i) = log( (1-R1_i(i)) + dnbinom2(Type(0.0), R2_i(i)*a_i(i), var_y, false)*R1_i(i) ); //  Pr[X=0] = 1-phi + NB(X=0)*phi
        }else{
          LogProb2_i(i) = dnbinom2(b_i(i), R2_i(i)*a_i(i), var_y, true) + log(R1_i(i)); // Pr[X=x] = NB(X=x)*phi
        }
      }
      LogProb1_i(i) = 0;
    }
    NLL -= LogProb1_i(i) + LogProb2_i(i);
  }

  // Predictive distribution -- ObsModel(4) isn't implemented (it had a bug previously)
  Type a_average = a_i.sum()/a_i.size();
  matrix<Type> P1_xt(n_x,n_t);
  matrix<Type> R1_xt(n_x,n_t);
  matrix<Type> P2_xt(n_x,n_t);
  matrix<Type> R2_xt(n_x,n_t);
  matrix<Type> D_xt(n_x,n_t);
  matrix<Type> Index_xt(n_x,n_t);
  vector<Type> Index_t(n_t);
  for(int t=0;t<n_t;t++){
    Index_t(t) = 0;
    for(int x=0;x<n_x;x++){
      P1_xt(x,t) = beta1_t(t) + Omega1_s(x) + Epsilon1_st(x,t) + eta1_x(x);
      R1_xt(x,t) = 1 / (1 + exp(-P1_xt(x,t)));
      P2_xt(x,t) =  beta2_t(t) + Omega2_s(x) + Epsilon2_st(x,t) + eta2_x(x);
      if(ObsModel(0)==0 | ObsModel(0)==1 | ObsModel(0)==2 | ObsModel(0)==4 | ObsModel(0)==5) R2_xt(x,t) = exp( P2_xt(x,t) );
      if(ObsModel(0)==11 | ObsModel(0)==12) R2_xt(x,t) = SigmaM(1)*exp(P2_xt(x,t)) + (1-SigmaM(1))*exp(P2_xt(x,t))*(1+SigmaM(2) );
      // Expected value for predictive distribution in a grid cell
      if(ObsModel(0)==0 | ObsModel(0)==1 | ObsModel(0)==2 | ObsModel(0)==5 | ObsModel(0)==11 | ObsModel(0)==12) D_xt(x,t) = R1_xt(x,t) * R2_xt(x,t);
      if(ObsModel(0)==4){
        var_y = R2_xt(x,t)*a_average*(1.0+SigmaM(0)) + pow(R2_xt(x,t)*a_average,2.0)*SigmaM(2);
        D_xt(x,t) = R1_xt(x,t) / (1-dnbinom2(Type(0.0), R2_xt(x,t)*a_average, var_y, false)) * R2_xt(x,t);
      }
      Index_xt(x,t) = D_xt(x,t) * a_x(x) / 1000;  // Convert from kg to metric tonnes
      Index_t(t) += Index_xt(x,t); 
    }
  }

  if( Options(0)==1 ){
    ADREPORT( P1_xt );
  }
  if( Options(1)==1 ){
    ADREPORT( P2_xt );
  }
  
  // Diagnostic output
  REPORT( P1_i );
  REPORT( P2_i );
  REPORT( R1_i );
  REPORT( R2_i );
  REPORT( P1_xt );
  REPORT( P2_xt );
  REPORT( LogProb1_i );
  REPORT( LogProb2_i );
  REPORT( a_average );
  
  REPORT( SigmaE1 );
  REPORT( SigmaO1 );
  REPORT( SigmaE2 );
  REPORT( SigmaO2 );
  REPORT( SigmaM );
  REPORT( SigmaV1 );
  REPORT( SigmaV2 );
  REPORT( SigmaVT1 );
  REPORT( SigmaVT2 );
  REPORT( Index_t );
  REPORT( D_xt );
  REPORT( R1_xt );
  REPORT( R2_xt );
  REPORT( Index_xt );
  REPORT( Omega1_s );
  REPORT( Omega2_s );
  REPORT( Epsilon1_st );
  REPORT( Epsilon2_st );
  REPORT( H );
  REPORT( Range_raw1 );
  //REPORT( Range_major1 );
  //REPORT( Range_minor1 );
  REPORT( Range_raw2 );
  //REPORT( Range_major2 );
  //REPORT( Range_minor2 );

  ADREPORT( Range_raw1 );
  ADREPORT( Range_raw2 );
  ADREPORT( Index_t );
  //ADREPORT( Index_xt );
  ADREPORT( log(Index_t) );
  ADREPORT( SigmaE1 );
  ADREPORT( SigmaO1 );
  ADREPORT( SigmaE2 );
  ADREPORT( SigmaO2 );
  ADREPORT( SigmaM );
  ADREPORT( SigmaV1 );
  ADREPORT( SigmaV2 );
  ADREPORT( SigmaVT1 );
  ADREPORT( SigmaVT2 );
  return NLL;
  
}
