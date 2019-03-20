
setwd("C:/Users/James.Thorson/Desktop/Git/FishStatsUtils/")
#library(mapdata)

# read file
bering_sea_slope_grid = read.csv( "data/SlopeThorsonGrid.csv" )

# Remove unnecessary columns
bering_sea_slope_grid = cbind( bering_sea_slope_grid, "Area_in_survey_km2"=bering_sea_slope_grid[,'Shape_Area']/1e6, "Depth_km"=bering_sea_slope_grid[,'ARDEMdepth']/1000 )
bering_sea_slope_grid = bering_sea_slope_grid[,c('Lat','Lon','Area_in_survey_km2','Depth_km')]

# Convert previous TXT format to new RDA format
devtools::use_data( bering_sea_slope_grid, pkg=getwd())

# Plot
maps::map( "worldHires" )
points( y=bering_sea_slope_grid[,'Lat'], x=bering_sea_slope_grid[,'Lon'])

