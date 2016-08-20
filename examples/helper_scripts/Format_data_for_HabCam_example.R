
setwd("C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/")
library(mapdata)

# read file
habcam_grid = read.csv( "C:/Users/James.Thorson/Desktop/UW Hideaway/NWFSC/2016-08 -- SpatialDeltaGLMM HabCam demo/Habcam_MAB_grid.csv" )

# Remove unnecessary columns
colnames(habcam_grid)[6] = "Zone_total_area_km2"
Zone_area_per_cell = tapply( habcam_grid[,'Zone_total_area_km2'], INDEX=habcam_grid[,'Zone'], FUN=function(vec){mean(vec)/length(vec)})
habcam_grid = cbind( habcam_grid, "Cell_area_km2"=Zone_area_per_cell[ match(habcam_grid[,'Zone'],names(Zone_area_per_cell)) ])

# Checksums
tapply( habcam_grid[,'Zone_total_area_km2'], INDEX=habcam_grid[,'Zone'], FUN=mean)
tapply( habcam_grid[,'Cell_area_km2'], INDEX=habcam_grid[,'Zone'], FUN=sum)

# Plot
maps::map( "worldHires", xlim=range(habcam_grid[,'Longitude']), ylim=range(habcam_grid[,'Latitude']) )
points( y=habcam_grid[,'Latitude'], x=habcam_grid[,'Longitude'])

# rename columns
colnames(habcam_grid)[1:2] = c("Lat","Lon")

# Convert previous TXT format to new RDA format
devtools::use_data( habcam_grid, pkg=getwd())

