
library(mapdata)
setwd("C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/")

# read file
gulf_of_mexico_grid = read.csv( "data/Lattice_US_GOM_FLRACEP_project_V3.csv", header=TRUE )

# Plot
map( "worldHires", xlim=range(gulf_of_mexico_grid[Which,'Lon']), ylim=range(gulf_of_mexico_grid[,'Lat']) )
points( y=gulf_of_mexico_grid[,'Lat'], x=gulf_of_mexico_grid[,'Lon'], col=ifelse(gulf_of_mexico_grid[,'Area_in_survey_km2']!=0,"black","red") )

# Convert previous TXT format to new RDA format
devtools::use_data( gulf_of_mexico_grid, pkg=getwd())

