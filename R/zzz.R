
#.onLoad <- function(libname, pkgname) {
#}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loading package SpatialDeltaGLMM, developed by James Thorson for the Northwest Fisheries Science Center")
  packageStartupMessage("For details and citation guidance, please see https://github.com/nwfsc-assess/geostatistical_delta-GLMM/#description-of-package")
  if( !"INLA" %in% installed.packages()[,1] ){
    message("Installing INLA...")
    install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")  
  }
  #if( !"TMB" %in% installed.packages()[,1] ){
  #  message("Installing TMB...")
  #  devtools::install_github("kaskr/adcomp/TMB")
  #}
  if( !"TMBhelper" %in% installed.packages()[,1] ){
    print("Installing package: TMBhelper...")
    devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
  }
}
