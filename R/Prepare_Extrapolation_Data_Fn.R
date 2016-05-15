
#' @export
Prepare_Extrapolation_Data_Fn = function( Region, strata.limits, observations_LL=NULL, ... ){

  Extrapolation_List = NULL
  if( Region == "California_current" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_WCGBTS_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "British_Columbia" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_BC_Coast_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Eastern_Bering_Sea" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_EBS_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Gulf_of_Alaska" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_GOA_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Northwest_Atlantic" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_NWA_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "South_Africa" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_SA_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Gulf_of_St_Lawrence" ){
    Extrapolation_List = SpatialDeltaGLMM::Prepare_GSL_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( is.null(Extrapolation_List) ){
    if( is.null(observations_LL)) message("Because you're using a new region, please provide 'observations_LL' input")
    Extrapolation_List = SpatialDeltaGLMM::Prepare_Other_Extrapolation_Data_Fn( strata.limits=strata.limits, observations_LL=observations_LL, ... )
  }

  # Return
  return( Extrapolation_List )
}
