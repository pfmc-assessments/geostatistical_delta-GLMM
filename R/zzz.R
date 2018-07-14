
#.onLoad <- function(libname, pkgname) {
#}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("###########################################################################################")
  packageStartupMessage("Loading package SpatialDeltaGLMM, developed by James Thorson for the Northwest Fisheries Science Center")
  packageStartupMessage("For details and citation guidance, please see https://github.com/nwfsc-assess/geostatistical_delta-GLMM/#description-of-package")
  packageStartupMessage("###########################################################################################")
  if( !"INLA" %in% utils::installed.packages()[,1] ){
    packageStartupMessage("Installing INLA...")
    utils::install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
  }
  #if( !"TMB" %in% utils::installed.packages()[,1] ){
  #  packageStartupMessage("Installing TMB...")
  #  devtools::install_github("kaskr/adcomp/TMB")
  #}
  #if( !"TMBhelper" %in% utils::installed.packages()[,1] ){
  #  packageStartupMessage("Installing package: TMBhelper...")
  #  devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
  #}
}

#' Copy of FishStatsUtils::make_extrapolation_data
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::Prepare_Extrapolation_Data_Fn} to see list of arguments and usage
#' @export
Prepare_Extrapolation_Data_Fn = function( ... ){
  FishStatsUtils::make_extrapolation_data( ... )
}

#' Copy of FishStatsUtils::make_spatial_info
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::make_spatial_info} to see list of arguments and usage
#' @export
Spatial_Information_Fn = function( ... ){
  FishStatsUtils::make_spatial_info( ... )
}

#' Copy of FishStatsUtils::make_map_info
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::make_map_info} to see list of arguments and usage
#' @export
MapDetails_Fn = function( ... ){
  FishStatsUtils::make_map_info( ... )
}

#' Copy of FishStatsUtils::plot_quantile_diagnostic
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_quantile_diagnostic} to see list of arguments and usage
#' @export
QQ_Fn = function( ... ){
  FishStatsUtils::plot_quantile_diagnostic( ... )
}

#' Copy of FishStatsUtils::plot_biomass_index
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_biomass_index} to see list of arguments and usage
#' @export
PlotIndex_Fn = function( ... ){
  FishStatsUtils::plot_biomass_index( ... )
}

#' Copy of FishStatsUtils::plot_maps
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_maps} to see list of arguments and usage
#' @export
PlotResultsOnMap_Fn = function( ... ){
  FishStatsUtils::plot_maps( ... )
}

#' Copy of FishStatsUtils::plot_encounter_diagnostic
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_encounter_diagnostic} to see list of arguments and usage
#' @export
Check_encounter_probability = function( ... ){
  FishStatsUtils::plot_encounter_diagnostic( ... )
}

#' Copy of FishStatsUtils::plot_residuals
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_residuals} to see list of arguments and usage
#' @export
plot_residuals = function( ... ){
  FishStatsUtils::plot_residuals( ... )
}

#' Copy of FishStatsUtils::plot_anisotropy
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_anisotropy} to see list of arguments and usage
#' @export
PlotAniso_Fn = function( ... ){
  FishStatsUtils::plot_anisotropy( ... )
}

#' Copy of FishStatsUtils::plot_range_index
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_range_index} to see list of arguments and usage
#' @export
Plot_range_shifts = function( ... ){
  FishStatsUtils::plot_range_index( ... )
}

#' Copy of FishStatsUtils::plot_data
#'
#' Included for continuity when using old scripts
#'
#' Please use \code{?FishStatsUtils::plot_data} to see list of arguments and usage
#' @export
Plot_data_and_knots = function( ... ){
  FishStatsUtils::plot_data( ... )
}
