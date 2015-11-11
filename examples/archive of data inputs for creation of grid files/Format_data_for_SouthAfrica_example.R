
SC = read.csv( "South Africa/SC_grid.csv" )
WC = read.csv( "South Africa/WC_grid.csv" )

south_africa_grid = rbind( data.frame(SC,"stratum"="south_coast"), data.frame(WC,"stratum"="west_coast") )
summary(south_africa_grid)

library( maps )
map( "world", xlim=range(south_africa_grid$cen_long), ylim=range(south_africa_grid$cen_lat) )
points( x=south_africa_grid$cen_long, y=south_africa_grid$cen_lat)

setwd( "C:/Users/Jim/Desktop/Project_git/geostatistical_delta-GLMM" )
devtools::use_data( south_africa_grid, pkg=getwd())
