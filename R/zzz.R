
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Welcome to SpatialDeltaGLMM")
  if( !"INLA" %in% installed.packages()[,1] ){
    install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to SpatialDeltaGLMM")
  if( !"INLA" %in% installed.packages()[,1] ){
    install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")  
  }
}
