Data_Fn <-
function( n_t, n_v, Aniso, FieldConfig, ObsModel, Options=c(0,0), b_i, a_i, v_i, s_i, t_i, a_xl, X_xj, Q_ik, MeshList, CheckForErrors=TRUE ){
  # Determine dimensions
  n_i = length(b_i)
  n_x = nrow(a_xl)
  n_j = ncol(X_xj)
  n_k = ncol(Q_ik)
  n_l = ncol(a_xl)
  # Check for bad data entry
  if( CheckForErrors==TRUE ){
    if( length(b_i)!=n_i | length(a_i)!=n_i | length(v_i)!=n_i | length(s_i)!=n_i | length(t_i)!=n_i ) error("b_i, a_i, v_i, s_i, or t_i doesn't have length n_i")
    if( nrow(a_xl)!=n_x | ncol(a_xl)!=n_l ) error("a_xl has wrong dimensions")
    if( nrow(X_xj)!=n_x | ncol(X_xj)!=n_j ) error("X_xj has wrong dimensions")
    if( nrow(Q_ik)!=n_i | ncol(Q_ik)!=n_k ) error("Q_ik has wrong dimensions")
  }
  # Output tagged list
  if(Version=="geo_index_v3a"){
    Return = list( "n_i"=n_i, "n_s"=MeshList$spde$n.spde, "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i, "a_x"=a_xl[,1], "X_xj"=X_xj, "Q_ik"=Q_ik, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0_inv"=inla.as.dgTMatrix(solve(MeshList$spde$param.inla$M0)), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version=="geo_index_v3b"){
    Return = list( "n_i"=n_i, "n_s"=MeshList$spde$n.spde, "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i, "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0_inv"=inla.as.dgTMatrix(solve(MeshList$spde$param.inla$M0)), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version=="geo_index_v3c"){
    Return = list( "n_i"=n_i, "n_s"=MeshList$spde$n.spde, "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i, "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "spde"=list(), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
    Return[['spde']] = list("n_s"=MeshList$spde$n.spde, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0"=MeshList$spde$param.inla$M0, "G0_inv"=inla.as.dgTMatrix(solve(MeshList$spde$param.inla$M0)) )
  }
  return( Return )
}
