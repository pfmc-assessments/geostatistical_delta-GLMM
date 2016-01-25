
Plot_data_and_knots = function( Data_Extrap, Extrap_Area_km2, loc_x, Data_Geostat, Plot_name=paste0(getwd(),"/Data_and_knots.png")){
  # Plot data and grid
  png( file=Plot_name, width=6, height=6, res=200, units="in")
    par( mfrow=c(2,2), mar=c(3,3,2,0), mgp=c(1.75,0.25,0) )
    plot( Data_Extrap[which(Extrap_Area_km2>0),c('Lon','Lat')], cex=0.01, main="Extrapolation (UTM)" )
    map( "world", add=TRUE )
    plot( Data_Extrap[which(Extrap_Area_km2>0),c('E_km','N_km')], cex=0.01, main="Extrapolation (North-East)" )
    plot( loc_x, col="red", pch=20, main="Knots (North-East)")
    plot( Data_Geostat[,c('E_km','N_km')], col="blue", pch=20, cex=0.1, main="Data (North-East)")
  dev.off()
}
