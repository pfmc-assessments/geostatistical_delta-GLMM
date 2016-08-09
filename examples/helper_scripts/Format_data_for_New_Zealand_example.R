
library( maps )
library( mapdata )

####################
# Extrapolation grid
####################

setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
load( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/examples/archive of data inputs for creation of grid files/NZ_grid_EEZ_CHAT_1km.R" )

chatham_rise_grid = data.frame( 'Lat'=as.vector(outGrid$gridData[,3]))
chatham_rise_grid = cbind( chatham_rise_grid, 'Lon'=unlist(outGrid$gridData[,4]))
chatham_rise_grid = cbind( chatham_rise_grid, 'area_km2'=1)
chatham_rise_grid = chatham_rise_grid[ which(outGrid$gridData[,'CHAT']==1), ]
map( "worldHires" )
points( x=chatham_rise_grid[,'Lon'], y=chatham_rise_grid[,'Lat'] )

summary( chatham_rise_grid )
devtools::use_data( chatham_rise_grid, pkg=getwd())
