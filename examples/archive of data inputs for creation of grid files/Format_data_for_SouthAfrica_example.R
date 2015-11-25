
SC = read.csv( "South Africa/SC_grid.csv" )
WC = read.csv( "South Africa/WC_grid.csv" )

south_africa_grid = rbind( data.frame(SC,"stratum"="south_coast"), data.frame(WC,"stratum"="west_coast") )
summary(south_africa_grid)

library( maps )
map( "world", xlim=range(south_africa_grid$cen_long), ylim=range(south_africa_grid$cen_lat) )
points( x=south_africa_grid$cen_long, y=south_africa_grid$cen_lat)

setwd( "C:/Users/Jim/Desktop/Project_git/geostatistical_delta-GLMM" )
devtools::use_data( south_africa_grid, pkg=getwd())

#######################
# Prepare CPUE data
#######################

TmbDir = "C:/Users/Jim/Desktop/Project_git/geostatistical_delta-GLMM/inst/executables/"
setwd( TmbDir )
south_africa_westcoast_jacopever = read.csv( paste0(getwd(),"/../../examples/archive of data inputs for creation of grid files/South Africa/SAWC_geodata.csv") )
south_africa_westcoast_jacopever = south_africa_westcoast_jacopever[which(south_africa_westcoast_jacopever$season=="Summer" & south_africa_westcoast_jacopever$vessel=="RV Africana" & south_africa_westcoast_jacopever$gear.used=="Dem_old"),]
south_africa_westcoast_jacopever = south_africa_westcoast_jacopever[,c('Year','cen_lat','cen_long','HELDAC','area_swept_nm2','start_depth_bottom_m','season','vessel','gear.used')]
devtools::use_data( south_africa_westcoast_jacopever, pkg=paste0(getwd(),"/../../"))

