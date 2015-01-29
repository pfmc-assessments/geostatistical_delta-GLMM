Data_Fn <-
function( n_i, n_x, n_t, n_v, n_j, n_k, n_l, Aniso, FieldConfig, ObsModel, Options=c(0,0), b_i, a_i, v_i, s_i, t_i, a_xl, X_xj, Q_ik, MeshList ){
  if(Version=="geo_index_v3b"){
    Return = list( "n_i"=n_i, "n_s"=MeshList$spde$n.spde, "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i, "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0_inv"=as(diag(1/diag(MeshList$spde$param.inla$M0)),"dgTMatrix"), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
  }
  if(Version=="geo_index_v3c"){
    Return = list( "n_i"=n_i, "n_s"=MeshList$spde$n.spde, "n_x"=n_x, "n_t"=n_t, "n_v"=n_v, "n_j"=n_j, "n_k"=n_k, "n_l"=n_l, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "ObsModel"=ObsModel, "Options"=Options, "b_i"=b_i, "a_i"=a_i, "v_i"=v_i, "s_i"=s_i, "t_i"=t_i, "a_xl"=a_xl, "X_xj"=X_xj, "Q_ik"=Q_ik, "spde"=list(), "G0"=MeshList$spde$param.inla$M0, "G1"=MeshList$spde$param.inla$M1, "G2"=MeshList$spde$param.inla$M2 )
    Return[['spde']] = list("n_s"=MeshList$spde$n.spde, "n_tri"=nrow(MeshList$mesh$graph$tv), "Tri_Area"=MeshList$Tri_Area, "E0"=MeshList$E0, "E1"=MeshList$E1, "E2"=MeshList$E2, "TV"=MeshList$TV-1, "G0"=MeshList$spde$param.inla$M0, "G0_inv"=as(diag(1/diag(MeshList$spde$param.inla$M0)),"dgTMatrix") )
  }
  return( Return )
}
