
MapDetails_Fn = function( Region, NN_Extrap, Extrapolation_List ){
  PlotDF = NULL
  if( Region == "California_current" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    #MappingDetails = list("state",c("Oregon","Washington","California"))
    MappingDetails = list("state", c("alabama","arizona","arkansas","california","colorado","connecticut","delaware","district of columbia","florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts:martha's vineyard","massachusetts:main","massachusetts:nantucket","michigan:north","michigan:south","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico","new york:manhattan","new york:main","new york:statenisland","new york:longisland","north carolina:knotts","north carolina:main","north carolina:spit","north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia:chesapeake","virginia:chincoteague","virginia:main","washington:san juan island","washington:lopez island","washington:orcas island","washington:whidbey island","washington:main","west virginia","wisconsin","wyoming"))
    Xlim=c(-126,-117)
    Ylim=c(32,49)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=1.55)
    Rotate = 20     # Degrees counter-clockwise
    Cex = 0.01
  }
  if( Region == "British_Columbia" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0) )
    MappingDetails = list("world", NULL)
    Xlim=c(-133,-126)
    Ylim=c(50,55)
    MapSizeRatio = c("Height(in)"=2,"Width(in)"=2)
    Rotate = 0
    Cex = 0.1
  }
  if( Region == "Eastern_Bering_Sea" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0) )
    PlotDF = PlotDF[which(PlotDF[,'Lon']<0),]
    MappingDetails = list("world", NULL)
    Xlim = c(-180,-158)
    Ylim=c(54,63)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=5)
    Rotate = 0
    Cex = 0.01
  }
  if( Region == "Gulf_of_Alaska" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("world", NULL)
    Xlim = c(-171,-132)
    Ylim=c(52,61)
    MapSizeRatio = c("Height(in)"=2.5,"Width(in)"=6)
    Rotate = 0
    Cex = 0.01
  }
  if( Region == "Northwest_Atlantic" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    #MappingDetails = list("world", NULL)
    MappingDetails = list("state", c("alabama","arizona","arkansas","california","colorado","connecticut","delaware","district of columbia","florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts:martha's vineyard","massachusetts:main","massachusetts:nantucket","michigan:north","michigan:south","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico","new york:manhattan","new york:main","new york:statenisland","new york:longisland","north carolina:knotts","north carolina:main","north carolina:spit","north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia:chesapeake","virginia:chincoteague","virginia:main","washington:san juan island","washington:lopez island","washington:orcas island","washington:whidbey island","washington:main","west virginia","wisconsin","wyoming"))
    Xlim = c(-80,-65)
    Ylim=c(32,45)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=3)
    Rotate = 0
    Cex = 0.01
  }
  if( Region == "South_Africa" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("world", NULL )
    Xlim = c(14,26)
    Ylim=c(-37,-28)
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=3)
    Rotate = 0
    Cex = 0.1
  }
  if( Region == "Gulf_of_St_Lawrence" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("worldHires", "Canada" )
    Xlim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lon'])
    Ylim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lat'])
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=4)
    Rotate = 0
    Cex = 1.0
  }
  if( is.null(PlotDF) ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=(Extrapolation_List[["Area_km2_x"]]>0))
    MappingDetails = list("world", NULL )
    Xlim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lon'])
    Ylim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lat'])
    MapSizeRatio = c("Height(in)"=4,"Width(in)"=4)
    Rotate = 0
    Cex = 1.0
  }
  # bundle and return
  mapdetails_list = list("PlotDF"=PlotDF, "MappingDetails"=MappingDetails, "Xlim"=Xlim, "Ylim"=Ylim, "MapSizeRatio"=MapSizeRatio, "Rotate"=Rotate, "Cex"=Cex )
  return( mapdetails_list )
}
