library(testthat)
library(SpatialDeltaGLMM)
library(TMB)

# Use "extdata" in "inst" because its loaded with R packages
example_path <- system.file("extdata", package="SpatialDeltaGLMM")

# Automated testing
setwd(system.file("tests", package="SpatialDeltaGLMM"))
testthat::test_check("SpatialDeltaGLMM")

# Local testing
if(FALSE){
  # Use local path
  example_path <- "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/inst/extdata/"
  # Run from local directory
  testthat::test_dir( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/tests/testthat/", reporter="check" )
}
