
#' @title
#' Plot location of sampling data
#'
#' @description
#' \code{Plot_data_and_knots} produces diagnostics plots for the spatial distribution of data and knots
#'
#' @param Extrapolation_List Output from \code{Prepare_Extrapolation_Data_Fn}
#' @param Spatial_List Output from \code{Spatial_Information_Fn}
#' @param Data_Geostat data-frame of data (with columns 'E_km', 'N_km', 'Year', 'Lon', 'Lat' at a minimum)
#' @param PlotDir Directory for plots
#' @param ... addition inputs to \code{plot}
#'

#' @export
Plot_data_and_knots = function( Extrapolation_List, Spatial_List, Data_Geostat, PlotDir=paste0(getwd(),"/"), Plot1_name="Data_and_knots.png", Plot2_name="Data_by_year.png", ...){

  # avoid attaching maps and mapdata to use worldHires plotting
  require(maps)
  require(mapdata)
  on.exit( detach("package:mapdata") )
  on.exit( detach("package:maps"), add=TRUE )

  # Plot data and grid
  png( file=paste0(PlotDir,Plot1_name), width=6, height=6, res=200, units="in")
    par( mfrow=c(2,2), mar=c(3,3,2,0), mgp=c(1.75,0.25,0) )
    plot( Extrapolation_List$Data_Extrap[which(Extrapolation_List$Area_km2_x>0),c('Lon','Lat')], cex=0.01, main="Extrapolation (Lat-Lon)" )
    map( "world", add=TRUE )
    plot( Extrapolation_List$Data_Extrap[which(Extrapolation_List$Area_km2_x>0),c('E_km','N_km')], cex=0.01, main="Extrapolation (North-East)" )
    plot( Spatial_List$loc_x, col="red", pch=20, main="Knots (North-East)")
    plot( Data_Geostat[,c('E_km','N_km')], col="blue", pch=20, cex=0.1, main="Data (North-East)")
  dev.off()

  # Plot data by year
  # Use Data_Geostat, instead of TmbData, because I want raw locations, not locations at knots
  Year_Set = min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])
    Nrow = ceiling( sqrt(length(Year_Set)) )
    Ncol = ceiling( length(Year_Set)/Nrow )
    if( is.null(Year_Set) ) Year_Set = Year_Set
  png( file=paste0(PlotDir,Plot2_name), width=Ncol*2, height=Nrow*2, res=200, units="in")
    par( mfrow=c(Nrow,Ncol), mar=c(0,0,2,0), mgp=c(1.75,0.25,0), oma=c(4,4,0,0) )
    for( t in 1:length(Year_Set) ){
      Which = which( Data_Geostat[,'Year'] == Year_Set[t] )
      plot( x=Data_Geostat[Which,'Lon'], y=Data_Geostat[Which,'Lat'], cex=0.01, main=Year_Set[t], xlim=range(Data_Geostat[,'Lon']), ylim=range(Data_Geostat[,'Lat']), xaxt="n", yaxt="n", ... )
      map( "world", add=TRUE )
      if( t>(length(Year_Set)-Ncol) ) axis(1)
      if( t%%Ncol == 1 ) axis(2)
      mtext( side=c(1,2), text=c("Longitude","Latitude"), outer=TRUE, line=1)
    }
  dev.off()
}
