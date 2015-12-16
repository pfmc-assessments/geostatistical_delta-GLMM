
MapDetails_Fn = function( Region, NN_Extrap, Extrapolation_List ){
  if( Region == "California_current" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("state",c("Oregon","Washington","California"))
    Xlim=c(-126,-117)
    Ylim=c(32,49)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=1.55)
    Rotate = 20
  }
  if( Region == "British_Columbia" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0) )
    MappingDetails = list("world", NULL)
    Xlim=c(-133,-126)
    Ylim=c(50,55)
    MapSizeRatio = c("Height(in)"=2,"Width(in)"=2)
    Rotate = 0
    Cex = 0.1
    Year_Set = unique(BC_pacific_cod_example[,'Year'])
  }
  if( Region == "Eastern_Bering_Sea" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0) )
    PlotDF = PlotDF[which(PlotDF[,'Lon']<0),]
    MappingDetails = list("world", NULL)
    Xlim = c(-180,-158)
    Ylim=c(54,63)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=5)
    Rotate = 0
  }
  if( Region == "Gulf_of_Alaska" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("world", NULL)
    Xlim = c(-171,-132)
    Ylim=c(52,61)
    MapSizeRatio = c("Height(in)"=2.5,"Width(in)"=6)
    Rotate = 0
  }
  if( Region == "Northwest_Atlantic" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("world", NULL)
    Xlim = c(-80,-65)
    Ylim=c(32,45)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=3)
    Rotate = 0
    Cex = 1.5
  }
  if( Region == "South_Africa" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("world", NULL)
    Xlim = c(14,26)
    Ylim=c(-37,-28)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=3)
    Rotate = 0
    Cex = 1.5
  }
  # bundle and return
  mapdetails_list = list("PlotDF"=PlotDF, "MappingDetails"=MappingDetails, "Xlim"=Xlim, "Ylim"=Ylim, "MapSizeRatio"=MapSizeRatio, "Rotate"=Rotate )
  return( mapdetails_list )
}
