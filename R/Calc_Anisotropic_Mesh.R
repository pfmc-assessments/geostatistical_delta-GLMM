
#' Make mesh for distances among points
#'
#' \code{Calc_Anistropic_Mesh} builds a tagged list representing distances for isotropic or geometric anisotropic triangulated mesh
#'
#' @param loc_x location (eastings and northings in kilometers, UTM) for each sample or knot
#' @param Method spatial method determines ("Mesh" and "Grid" give
#' @param anisotropic_mesh OPTIONAL, anisotropic mesh (if missing, its recalculated from loc_x)
#' @param refine OPTIONAL, specify whether to add additional points (beyond loc_x and minimal boundary knots)
#' @param ... Arguments passed to \code{INLA::inla.mesh.create}

#' @return Tagged list containing distance metrics

#' @export
Calc_Anisotropic_Mesh <-
function(loc_x, Method="Mesh", anisotropic_mesh=NULL, refine=FALSE, ...){

  #######################
  # Create the anisotropic SPDE mesh using 2D coordinates
  #######################

  # 2D coordinates SPDE
  if( is.null(anisotropic_mesh)) anisotropic_mesh = INLA::inla.mesh.create( loc_x, plot.delay=NULL, refine=refine, ...)
  anisotropic_spde = INLA::inla.spde2.matern(anisotropic_mesh, alpha=2)

  # Pre-processing in R for anisotropy
  Dset = 1:2
  # Triangle info
  TV = anisotropic_mesh$graph$tv       # Triangle to vertex indexing
  V0 = anisotropic_mesh$loc[TV[,1],Dset]   # V = vertices for each triangle
  V1 = anisotropic_mesh$loc[TV[,2],Dset]
  V2 = anisotropic_mesh$loc[TV[,3],Dset]
  E0 = V2 - V1                      # E = edge for each triangle
  E1 = V0 - V2
  E2 = V1 - V0
  
  # Calculate Areas 
  crossprod_fn = function(Vec1,Vec2) abs(det( rbind(Vec1,Vec2) ))
  Tri_Area = rep(NA, nrow(E0))
  for(i in 1:length(Tri_Area)) Tri_Area[i] = crossprod_fn( E0[i,],E1[i,] )/2   # T = area of each triangle

  ################
  # Add the isotropic SPDE mesh for spherical or 2D projection, depending upon `Method` input
  ################

  # Mesh and SPDE for different inputs
  if(Method %in% c("Mesh","Grid")){
    loc_isotropic_mesh = loc_x
  }
  if(Method %in% c("Spherical_mesh")){
    loc_isotropic_mesh = INLA::inla.mesh.map(loc_x, projection="longlat", inverse=TRUE) # Project from lat/long to mesh coordinates
  }
  isotropic_mesh = INLA::inla.mesh.create( loc_isotropic_mesh, plot.delay=NULL, refine=refine, ...)
  isotropic_spde = INLA::inla.spde2.matern(isotropic_mesh, alpha=2)

  ####################
  # Return stuff
  ####################
  if( isotropic_mesh$n != anisotropic_mesh$n ) stop("Check `Calc_Anisotropic_Mesh` for problem")

  Return = list("loc_x"=loc_x, "loc_isotropic_mesh"=loc_isotropic_mesh, "isotropic_mesh"=isotropic_mesh, "isotropic_spde"=isotropic_spde, "anisotropic_mesh"=anisotropic_mesh, "anisotropic_spde"=anisotropic_spde, "Tri_Area"=Tri_Area, "TV"=TV, "E0"=E0, "E1"=E1, "E2"=E2 )
  return(Return)
}
