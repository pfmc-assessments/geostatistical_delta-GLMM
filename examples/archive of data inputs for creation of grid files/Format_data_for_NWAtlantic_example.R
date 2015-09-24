
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )

######################
# Species
######################
WCGBTS_Canary_example <- NWFSC_Trawl
WCGBTS_Canary_example <- WCGBTS_Canary_example[,c('SPECIES','PROJECT_CYCLE','VESSEL','BEST_DEPTH_M','BEST_LAT_DD','BEST_LON_DD','AREA_SWEPT_HA','HAUL_WT_KG','PASS')]

WCGBTS_Canary_example[,'VESSEL'] = paste0( "Vessel_",letters[as.numeric(WCGBTS_Canary_example[,'VESSEL'])] )
devtools::use_data( WCGBTS_Canary_example, pkg=getwd())

# Convert previous TXT format to new RDA format
data( extrapolation_data )
devtools::use_data( extrapolation_data, pkg=getwd())

#######################
# Grid
#######################

northwest_atlantic_grid <- read.table( "examples/archive of data inputs for creation of grid files/NWAgrid2nmx2nm.txt", sep=",", header=TRUE)
northwest_atlantic_grid = northwest_atlantic_grid[c('STRATUMA','CenterX','CenterY','AreaSQkm')]
colnames(northwest_atlantic_grid) = sapply(colnames(northwest_atlantic_grid), FUN=function(char,...){ switch(char, ..., char)}, 'STRATUMA'="stratum_area", 'CenterX'="Lon", 'CenterY'="Lat", 'AreaSQkm'="Area_in_survey_km2")

Which = which( northwest_atlantic_grid[,'Area_in_survey_km2']>0 )
plot( x=northwest_atlantic_grid[Which,'Lon'], y=northwest_atlantic_grid[,'Lat'])

# Output as RDA format
devtools::use_data( northwest_atlantic_grid, pkg=getwd())
