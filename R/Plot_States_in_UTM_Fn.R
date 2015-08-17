
Plot_States_in_UTM_Fn = function( MappingDetails, Rotate=0, fillcol=NA, ... ){
  require(maps)
  require(PBSmapping)
  require(maptools)
  Map = map(MappingDetails[[1]], MappingDetails[[2]], plot=FALSE, fill=TRUE) # , orientation=c(mean(y.lim),mean(x.lim),15)
  Tmp1 = na.omit( cbind('PID'=cumsum(is.na(Map$x)), 'POS'=1:length(Map$x), 'X'=Map$x, 'Y'=Map$y ))
  # Convert_LL_to_UTM_Fn
  attr(Tmp1,"projection") = "LL"
  attr(Tmp1,"zone") = "10"
  tmpUTM = suppressMessages(convUL(Tmp1))                                                         #$
  coordinates(tmpUTM) = c("X","Y")
  tmp <- elide( tmpUTM, rotate=Rotate)
  # Plot map
  plot(1, pch="", ... )
  lev = levels(as.factor(tmp@data$PID))
  for(levI in 1:(length(lev))) {
    indx = which(tmpUTM$PID == lev[levI])
    polygon(tmp@coords[indx,'x'], tmp@coords[indx,'y'], col=fillcol)
  }
}
