
#' Make mesh for distances among points
#'
#' \code{Calc_Anistropic_Mesh} builds a tagged list representing distances for isotropic or geometric anisotropic triangulated mesh
#'
#' @param loc_x location (eastings and northings in kilometers, UTM) for each sample or knot
#' @param mesh OPTIONAL, isotropic mesh (if missing, its recalculated from loc_x)
#' @param refine OPTIONAL, specify whether to add additional points (beyond loc_x and minimal boundary knots)

#' @return Tagged list containing distance metrics

#' @export
Calc_Anisotropic_Mesh <-
function(loc_x, mesh=NULL, refine=FALSE, ...){
  # Create the SPDE mesh
  if( is.null(mesh)) mesh = INLA::inla.mesh.create( loc_x, plot.delay=NULL, refine=refine, ...)
  spde = INLA::inla.spde2.matern(mesh, alpha=2)

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
  crossprod_fn = function(Vec1,Vec2) abs(det( rbind(Vec1,Vec2) ))
  Tri_Area = rep(NA, nrow(E0))
  for(i in 1:length(Tri_Area)) Tri_Area[i] = crossprod_fn( E0[i,],E1[i,] )/2   # T = area of each triangle

  # Return stuff
  Return = list("loc_x"=loc_x, "Tri_Area"=Tri_Area, "TV"=TV, "E0"=E0, "E1"=E1, "E2"=E2, "mesh"=mesh, "spde"=spde)
  return(Return)
}
