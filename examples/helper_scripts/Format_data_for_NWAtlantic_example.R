
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )

######################
# Species
######################

georges_bank_haddock_spring <- read.csv( "examples/archive of data inputs for creation of grid files/NWA haddock/HADDOCK.GBK.Spring.DropZeroWt.TRUE.csv" )
georges_bank_haddock_spring = georges_bank_haddock_spring[,c("STRATUM","YEAR","MONTH","DAY","TIME","DEPTH","BOT_TEMP","CATCH_WT_CAL","CATCH_NO_CAL","LATITUDE","LONGITUDE")]
georges_bank_haddock_fall <- read.csv( "examples/archive of data inputs for creation of grid files/NWA haddock/HADDOCK.GBK.Fall.DropZeroWt.TRUE.csv" )
georges_bank_haddock_fall = georges_bank_haddock_fall[,c("STRATUM","YEAR","MONTH","DAY","TIME","DEPTH","BOT_TEMP","CATCH_WT_CAL","CATCH_NO_CAL","LATITUDE","LONGITUDE")]

devtools::use_data( georges_bank_haddock_spring, pkg=getwd())
devtools::use_data( georges_bank_haddock_fall, pkg=getwd())

#######################
# Grid
#######################

northwest_atlantic_grid <- read.table( "examples/archive of data inputs for creation of grid files/NWAgrid2nmx2nm.txt", sep=",", header=TRUE)
northwest_atlantic_grid = northwest_atlantic_grid[c('STRATUMA','CenterX','CenterY','AreaSQkm')]
colnames(northwest_atlantic_grid) = sapply(colnames(northwest_atlantic_grid), FUN=function(char,...){ switch(char, ..., char)}, 'STRATUMA'="stratum_number", 'CenterX'="Lon", 'CenterY'="Lat", 'AreaSQkm'="Area_in_survey_km2")

# Sanity check
sum(unique( northwest_atlantic_grid[,'stratum_area'] ))
sum(unique( northwest_atlantic_grid[,'Area_in_survey_km2'] ))

# Plot
Which = which( northwest_atlantic_grid[,'Area_in_survey_km2']>0 )
plot( x=northwest_atlantic_grid[Which,'Lon'], y=northwest_atlantic_grid[,'Lat'])

# Output as RDA format
devtools::use_data( northwest_atlantic_grid, pkg=getwd())

# Sanity check for Georges Bank haddock area
library( SpatialDeltaGLMM)
data( northwest_atlantic_grid )

# Sizes (from Liz Brooks)
Areas = read.csv( paste0(getwd(),"/examples/archive of data inputs for creation of grid files/strata_area_NWA_region.csv") )
Which = which( Areas$STRATUM %in% c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300))
sum( Areas$STRATUM_AREA[Which] ) * 100 * 4 * 1.852^2 # Convert square-nautical-mile to square-km
