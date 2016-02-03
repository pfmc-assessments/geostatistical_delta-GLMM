
Plot_data_and_knots = function( Data_Extrap, Extrap_Area_km2, loc_x, Data_Geostat, Year_Set=NULL, Plot_name=paste0(getwd(),"/Data_and_knots.png"), Data_name=paste0(getwd(),"/Data_by_year.png"), ...){

  # Plot data and grid
  png( file=Plot_name, width=6, height=6, res=200, units="in")
    par( mfrow=c(2,2), mar=c(3,3,2,0), mgp=c(1.75,0.25,0) )
    plot( Data_Extrap[which(Extrap_Area_km2>0),c('Lon','Lat')], cex=0.01, main="Extrapolation (UTM)" )
    map( "world", add=TRUE )
    plot( Data_Extrap[which(Extrap_Area_km2>0),c('E_km','N_km')], cex=0.01, main="Extrapolation (North-East)" )
    plot( loc_x, col="red", pch=20, main="Knots (North-East)")
    plot( Data_Geostat[,c('E_km','N_km')], col="blue", pch=20, cex=0.1, main="Data (North-East)")
  dev.off()

  # Plot data by year
  Years_in_data = sort(unique(Data_Geostat$Year))
    Nrow = ceiling( sqrt(length(Years_in_data)) )
    Ncol = ceiling( length(Years_in_data)/Nrow )
    if( is.null(Year_Set) ) Year_Set = Years_in_data
  png( file=Data_name, width=Ncol*2, height=Nrow*2, res=200, units="in")
    par( mfrow=c(Nrow,Ncol), mar=c(0,0,2,0), mgp=c(1.75,0.25,0), oma=c(4,4,0,0) )
    for( t in 1:length(Years_in_data) ){
      Which = which( Data_Geostat$Year == Years_in_data[t] )
      plot( x=Data_Geostat[Which,'Lon'], y=Data_Geostat[Which,'Lat'], cex=0.01, main=Year_Set[t], xlim=range(Data_Geostat[,'Lon']), ylim=range(Data_Geostat[,'Lat']), xaxt="n", yaxt="n", ... )
      map( "world", add=TRUE )
      if( t>(length(Years_in_data)-Ncol) ) axis(1)
      if( t%%Ncol == 1 ) axis(2)
      mtext( side=c(1,2), text=c("Longitude","Latitude"), outer=TRUE, line=1)
    }
  dev.off()

}
