
#' @export
MapDetails_Fn = function( Region, NN_Extrap, Extrapolation_List, Include=(Extrapolation_List[["Area_km2_x"]]>0&Extrapolation_List[["a_el"]][,1]>0) ){

  PlotDF = NULL

  if( Region == "California_current" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include)
    MappingDetails = list("state", c("alabama","arizona","arkansas","california","colorado","connecticut","delaware","district of columbia","florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts:martha's vineyard","massachusetts:main","massachusetts:nantucket","michigan:north","michigan:south","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico","new york:manhattan","new york:main","new york:statenisland","new york:longisland","north carolina:knotts","north carolina:main","north carolina:spit","north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia:chesapeake","virginia:chincoteague","virginia:main","washington:san juan island","washington:lopez island","washington:orcas island","washington:whidbey island","washington:main","west virginia","wisconsin","wyoming"))
    Xlim=c(-126,-117)
    Ylim=c(32,49)
    #MapSizeRatio = c("Height(in)"=4,"Width(in)"=1.55)
    Rotate = 20     # Degrees counter-clockwise
    Cex = 0.01
    Legend = list(use=TRUE, x=c(65,75), y=c(35,65))
    Zone = NA
  }
  if( Region == "British_Columbia" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    MappingDetails = list("worldHires", NULL)
    Xlim=c(-133,-126)
    Ylim=c(50,55)
    #MapSizeRatio = c("Height(in)"=2,"Width(in)"=2)
    Rotate = 0
    Cex = 0.1
    Legend = list(use=FALSE,x=c(10,30),y=c(10,30))
    Zone = NA
  }
  if( Region == "Eastern_Bering_Sea" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    PlotDF = PlotDF[which(PlotDF[,'Lon']<0),]
    MappingDetails = list("worldHires", NULL)
    Xlim = c(-180,-158)
    Ylim=c(54,63)
    #MapSizeRatio = c("Height(in)"=4,"Width(in)"=5)
    Rotate = 0
    Cex = 0.01
    Legend = list(use=TRUE,x=c(76,86),y=c(48,83))
    Zone = 3
  }
  if( Region == "Aleutian_Islands" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    PlotDF[,'Lon'] = PlotDF[,'Lon'] %% 360 # Change units to match world2Hires
    MappingDetails = list("world2Hires", NULL)
    Xlim = c(170,195)
    Ylim=c(51,55)
    #MapSizeRatio = c("Height(in)"=2,"Width(in)"=5)
    Rotate = 0
    Cex = 0.01
    Legend = list(use=FALSE,x=c(10,30),y=c(10,30))
    Zone = NA
  }
  if( Region == "Gulf_of_Alaska" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    MappingDetails = list("world", NULL)
    Xlim = c(-171,-132)
    Ylim=c(52,61)
    #MapSizeRatio = c("Height(in)"=2.5,"Width(in)"=6)
    Rotate = 0
    Cex = 0.01
    Legend = list(use=TRUE,x=c(5,10),y=c(30,65))
    Zone = NA
  }
  if( Region == "Northwest_Atlantic" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    #MappingDetails = list("world", NULL)
    MappingDetails = list("state", c("alabama","arizona","arkansas","california","colorado","connecticut","delaware","district of columbia","florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts:martha's vineyard","massachusetts:main","massachusetts:nantucket","michigan:north","michigan:south","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico","new york:manhattan","new york:main","new york:statenisland","new york:longisland","north carolina:knotts","north carolina:main","north carolina:spit","north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia:chesapeake","virginia:chincoteague","virginia:main","washington:san juan island","washington:lopez island","washington:orcas island","washington:whidbey island","washington:main","west virginia","wisconsin","wyoming"))
    Xlim = c(-80,-65)
    Ylim=c(32,45)
    #MapSizeRatio = c("Height(in)"=4,"Width(in)"=3)
    Rotate = 0
    Cex = 1
    Legend = list(use=TRUE,x=c(70,80),y=c(5,35))
    Zone = NA
  }
  if( Region == "South_Africa" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    MappingDetails = list("worldHires", NULL )
    Xlim = c(14,26)
    Ylim=c(-37,-28)
    #MapSizeRatio = c("Height(in)"=4,"Width(in)"=3)
    Rotate = 0
    Cex = 0.1
    Legend = list(use=FALSE,x=c(10,30),y=c(10,30))
    Zone = NA
  }
  if( Region == "Gulf_of_St_Lawrence" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    MappingDetails = list("worldHires", "Canada" )
    Xlim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lon'])
    Ylim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lat'])
    #MapSizeRatio = c("Height(in)"=4,"Width(in)"=4)
    Rotate = 0
    Cex = 1.0
    Legend = list(use=FALSE,x=c(10,30),y=c(10,30))
    Zone = NA
  }
  if( Region == "New_Zealand" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    MappingDetails = list("worldHires", NULL )
    Xlim=c(172,187)
    Ylim=c(-46,-41)
    #MapSizeRatio = c("Height(in)"=2,"Width(in)"=5)
    Rotate = 0     # Degrees counter-clockwise
    Cex = 0.01
    Legend = list(use=FALSE,x=c(10,30),y=c(10,30))
    Zone = NA
  }
  if( Region == "HabCam" ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include)
    #MappingDetails = list("state", c("alabama","arizona","arkansas","california","colorado","connecticut","delaware","district of columbia","florida","georgia","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts:martha's vineyard","massachusetts:main","massachusetts:nantucket","michigan:north","michigan:south","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico","new york:manhattan","new york:main","new york:statenisland","new york:longisland","north carolina:knotts","north carolina:main","north carolina:spit","north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia:chesapeake","virginia:chincoteague","virginia:main","washington:san juan island","washington:lopez island","washington:orcas island","washington:whidbey island","washington:main","west virginia","wisconsin","wyoming"))
    MappingDetails = list("worldHires", NULL )
    Xlim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lon'])
    Ylim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lat'])
    #MapSizeRatio = c("Height(in)"=4,"Width(in)"=3)
    Rotate = 20     # Degrees counter-clockwise
    Cex = 0.01
    Legend = list(use=TRUE,x=c(70,90),y=c(5,35))
    Zone = NA
  }
  if( is.null(PlotDF) ){
    PlotDF = cbind( Extrapolation_List[["Data_Extrap"]][,c('Lat','Lon')], 'x2i'=NN_Extrap$nn.idx, 'Include'=Include )
    MappingDetails = list("worldHires", NULL )
    Xlim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lon'])
    Ylim = range(Extrapolation_List[["Data_Extrap"]][which(Extrapolation_List[["Area_km2_x"]]>0),'Lat'])
    Rotate = 0
    Cex = 1.0
    Legend = list(use=FALSE,x=c(10,30),y=c(10,30))
    Zone = Extrapolation_List[["zone"]]
  }

  # Determine map size (equal distance along x-axis and y-axis)
  MapSizeRatio = c("Height(in)"=diff(range(Extrapolation_List$Data_Extrap[,'N_km'])) , "Width(in)"=diff(range(Extrapolation_List$Data_Extrap[,'E_km'])) )
  MapSizeRatio = MapSizeRatio / sqrt(prod(MapSizeRatio)) * 4  # 14 square-inches

  # bundle and return
  mapdetails_list = list("PlotDF"=PlotDF, "MappingDetails"=MappingDetails, "Xlim"=Xlim, "Ylim"=Ylim, "MapSizeRatio"=MapSizeRatio, "Rotate"=Rotate, "Cex"=Cex, "Legend"=Legend, "Zone"=Zone )
  return( mapdetails_list )
}
