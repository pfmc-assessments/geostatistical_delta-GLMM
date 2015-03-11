

WCGBTS_Canary_example <- NWFSC_Trawl

WCGBTS_Canary_example <- WCGBTS_Canary_example[,c('SPECIES','PROJECT_CYCLE','VESSEL','BEST_DEPTH_M','BEST_LAT_DD','BEST_LON_DD','AREA_SWEPT_HA','HAUL_WT_KG','PASS')]

WCGBTS_Canary_example[,'VESSEL'] = paste0( "Vessel_",letters[as.numeric(WCGBTS_Canary_example[,'VESSEL'])] )
write.table( WCGBTS_Canary_example, file="WCGBTS_Canary_example.txt", sep=" ", row.names=TRUE)

