library(testthat)
library(SpatialDeltaGLMM)
library(TMB)

# Use "extdata" in "inst" because its loaded with R packages
example_path <- system.file("extdata", package="SpatialDeltaGLMM")

# Automated testing
testthat::test_check("SpatialDeltaGLMM")

# Local testing
  # Use local path
  #example_path <- "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/inst/extdata/"

  # Run from local directory
  #testthat::test_dir( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/tests/testthat/", reporter="check" )
  #testthat::test_dir( "/media/sf_c/Users/jim/Desktop/Project_git/FishData/tests/testthat/", reporter="check" )

  # Full build check
  #devtools::check( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/" )
  #devtools::check( "/media/sf_c/Users/jim/Desktop/Project_git/FishData/ )

  # Update documentation
  #devtools::document( "C:/Users/James.Thorson/Desktop/Project_git/FishData/" )
