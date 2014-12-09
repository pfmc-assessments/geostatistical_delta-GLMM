Calc_Anisotropic_Mesh <-
function(loc_x){
  # Create the SPDE mesh
  mesh = inla.mesh.create( loc_x, plot.delay=NULL, refine=FALSE)
  spde = inla.spde2.matern(mesh, alpha=2)

  # Pre-processing in R for anisotropy
  Dset = 1:2
  # Triangle info
  TV = mesh$graph$tv       # Triangle to vertex indexing
  V0 = mesh$loc[TV[,1],Dset]   # V = vertices for each triangle
  V1 = mesh$loc[TV[,2],Dset]
  V2 = mesh$loc[TV[,3],Dset]
  E0 = V2 - V1                      # E = edge for each triangle
  E1 = V0 - V2
  E2 = V1 - V0
  
  # Calculate Areas
  TmpFn = function(Vec1,Vec2) abs(det( rbind(Vec1,Vec2) ))
  Tri_Area = rep(NA, nrow(E0))
  for(i in 1:length(Tri_Area)) Tri_Area[i] = TmpFn( E0[i,],E1[i,] )/2   # T = area of each triangle

  # Return stuff
  Return = list("Tri_Area"=Tri_Area, "TV"=TV, "E0"=E0, "E1"=E1, "E2"=E2, "mesh"=mesh, "spde"=spde)
  return(Return)
}
