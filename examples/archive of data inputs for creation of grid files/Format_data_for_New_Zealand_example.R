
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
library( ThorsonUtilities )

new_zealand_grid <- read.csv( "C:/Users/James.Thorson/Desktop/New Zealand travel files/Collaborations/2016 -- New Zealand example/chatham_rise_grid/ChathamRiseGridEA_CHAT_n=500.csv" )

# Convert previous TXT format to new RDA format
devtools::use_data( new_zealand_grid, pkg=getwd())

####################
# Extrapolation grid
####################
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
load( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/examples/archive of data inputs for creation of grid files/NZgrid/grids/NZ_grid_CHAT_1km_withGridPolygons.R" )
chatham_rise_grid = data.frame( 'Lat'=as.vector(chatGridP$gridData[,3]))
chatham_rise_grid = cbind( chatham_rise_grid, 'Lon'=unlist(chatGridP$gridData[,4]))
chatham_rise_grid = cbind( chatham_rise_grid, 'area_km2'=1)
summary( chatham_rise_grid )
devtools::use_data( chatham_rise_grid, pkg=getwd())
