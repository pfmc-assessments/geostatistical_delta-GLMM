
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )

######################
# Species
######################

georges_bank_haddack_spring <- read.csv( "examples/archive of data inputs for creation of grid files/NWA haddack/haddock_gis_spring68to08_saga42.csv" )
georges_bank_haddack_spring = georges_bank_haddack_spring[,c("Strata","Year","Mon","Day","Time","Depth","Bot_Temp","CatchWt","CatchNum","Latitude","Longitude")]
georges_bank_haddack_fall <- read.csv( "examples/archive of data inputs for creation of grid files/NWA haddack/haddock_gis_fall63to08_saga42.csv" )
georges_bank_haddack_fall = georges_bank_haddack_fall[,c("Strata","Year","Mon","Day","Time","Depth","Bot_Temp","CatchWt","CatchNum","Latitude","Longitude")]

devtools::use_data( georges_bank_haddack_spring, pkg=getwd())
devtools::use_data( georges_bank_haddack_fall, pkg=getwd())

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
