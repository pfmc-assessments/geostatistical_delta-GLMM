Calc_Polygon_Areas_and_Polygons_Fn <-
function( loc_x, Data_Extrap, Covariates="none", a_el=NULL ){

  # Calculate a_el if missing
  if( is.null(a_el) ){
    a_el = rep(1,nrow(Data_Extrap))
  }

  # Nearest extrapolation grid for each knot
  NN_Extrap = nn2( data=loc_x[,c('E_km','N_km')], query=Data_Extrap[,c('E_km','N_km')], k=1 )

  # Calculate area for each knot
  a_xl = matrix(NA, ncol=ncol(a_el), nrow=nrow(loc_x), dimnames=list(NULL,colnames(a_el)))
  for(l in 1:ncol(a_xl)){
    a_xl[,l] = tapply(a_el[,l], INDEX=factor(NN_Extrap$nn.idx,levels=1:nrow(loc_x)), FUN=sum)
    a_xl[,l] = ifelse( is.na(a_xl[,l]), 0, a_xl[,l] )
  }

  # Calculate covariate for each knot
  # Covariate j at location x is the average value in the extrapolation_grid for all grid cells s that are nearest to location x
  if( length(Covariates)==1 && Covariates=="none" ){
    X_xj = cbind( "Dummy"=rep(0,nrow(loc_x)) )
  }else{
    X_xj = matrix(NA, ncol=length(Covariates), nrow=nrow(loc_x))
    for(j in 1:ncol(X_xj)){
      X_xj[,j] = tapply(Data_Extrap[,Covariates[j]], INDEX=factor(NN_Extrap$nn.idx,levels=1:nrow(loc_x)), FUN=mean, na.rm=TRUE)
    }
  }

  # Return stuff
  Return = list( "X_xj"=X_xj, "a_xl"=a_xl, "NN_Extrap"=NN_Extrap )
}
