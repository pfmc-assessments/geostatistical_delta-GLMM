
#.onLoad <- function(libname, pkgname) {
#}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loading package SpatialDeltaGLMM, developed by James Thorson for the Northwest Fisheries Science Center")
  packageStartupMessage("For details and citation guidance, please see https://github.com/nwfsc-assess/geostatistical_delta-GLMM/#description-of-package")
  if( !"INLA" %in% installed.packages()[,1] ){
    install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")  
  }
  if( !"TMB" %in% installed.packages()[,1] ){
    devtools::install_github("kaskr/adcomp/TMB")
  }
}
