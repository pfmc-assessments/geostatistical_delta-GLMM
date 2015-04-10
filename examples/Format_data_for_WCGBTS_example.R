
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )

WCGBTS_Canary_example <- NWFSC_Trawl

WCGBTS_Canary_example <- WCGBTS_Canary_example[,c('SPECIES','PROJECT_CYCLE','VESSEL','BEST_DEPTH_M','BEST_LAT_DD','BEST_LON_DD','AREA_SWEPT_HA','HAUL_WT_KG','PASS')]

WCGBTS_Canary_example[,'VESSEL'] = paste0( "Vessel_",letters[as.numeric(WCGBTS_Canary_example[,'VESSEL'])] )
devtools::use_data( WCGBTS_Canary_example, pkg=getwd())

# Convert previous TXT format to new RDA format
data( extrapolation_data )
devtools::use_data( extrapolation_data, pkg=getwd())

