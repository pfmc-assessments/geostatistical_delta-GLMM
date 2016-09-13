
#' Build extrapolation grid
#'
#' \code{Prepare_Extrapolation_Data_Fn} builds an object used to determine areas to extrapolation densities to when calculating indices
#'
#' @param Region a character entry that is matched against potential values to determine the region for the extrapolation grid.
#' @param strata.limits an input for determining stratification of indices (see example script)
#' @param observations_LL a matrix with two columns (labeled 'Lat' and 'Lon') giving latitude and longitude for each observation (only used when Region doesn't match known entries)
#' @param ... other objects passed for individual regions (see example script)

#' @return Tagged list used in other functions
#' \describe{
#'   \item{a_el}{The area associated with each extrapolation grid cell (rows) and strata (columns)}
#'   \item{Data_Extrap}{A data frame describing the extrapolation grid}
#'   \item{zone}{the zone used to convert Lat-Long to UTM by PBSmapping package}
#'   \item{flip_around_dateline}{a boolean stating whether the Lat-Long is flipped around the dateline during conversion to UTM}
#'   \item{Area_km2_x}{the area associated with each row of Data_Extrap, in units square-kilometers}
#' }

#' @export
Prepare_Extrapolation_Data_Fn = function( Region, strata.limits, observations_LL=NULL, ... ){

  Extrapolation_List = NULL
  if( Region == "California_current" ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_WCGBTS_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region %in% c("WCGHL","WCGHL_domain") ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_WCGHL_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }                      #
  if( Region == "British_Columbia" ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_BC_Coast_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Eastern_Bering_Sea" ){ #
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_EBS_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Aleutian_Islands" ){ #
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_AI_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Gulf_of_Alaska" ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_GOA_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Northwest_Atlantic" ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_NWA_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "South_Africa" ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_SA_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "Gulf_of_St_Lawrence" ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_GSL_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "New_Zealand" ){
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_NZ_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( Region == "HabCam" ){  #
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_HabCam_Extrapolation_Data_Fn( strata.limits=strata.limits, ... )
  }
  if( is.null(Extrapolation_List) ){
    if( is.null(observations_LL)) message("Because you're using a new region, please provide 'observations_LL' input")
    Extrapolation_List = SpatialDeltaGLMM:::Prepare_Other_Extrapolation_Data_Fn( strata.limits=strata.limits, observations_LL=observations_LL, ... )
  }

  # Return
  return( Extrapolation_List )
}
