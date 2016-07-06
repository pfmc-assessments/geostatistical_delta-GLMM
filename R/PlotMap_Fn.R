#' @title
#' Plot maps with areal results
#'
#' @description
#' \code{PlotMap_Fn} is a hidden function to plot a map and fill in regions with colors to represent intensity in an areal-interpretion of model results
#'
#' @details
#' This function was necessary to buiild because \code{mapproj::mapproject} as used in \code{maps::map} has difficulties with both rotations (for most projections) and
#' truncating the cocuntry boundaries within the plotting region (which \code{mapproj::mapproject} appears to do prior to projection,
#' so that the post-projection is often missing boundaries that are within the plotting rectangle).  I use rectangular projections by default, but Lamberts or Albers conformal
#' projections would also be useful for many cases.

PlotMap_Fn <-
function(MappingDetails, Mat, PlotDF, MapSizeRatio, Xlim, Ylim, FileName, Year_Set,
         Rescale=FALSE, Rotate=0, Format="png", Res=200, zone=NA, Cex=0.01, textmargin="", add=FALSE, pch=20, outermargintext=c("Eastings","Northings"), zlim=NULL, ...){

  # maps and mapdata must be attached to use worldHires plotting

  # Transform to grid or other coordinates
  Mat = Mat[PlotDF[,'x2i'],,drop=FALSE]
  Which = which( PlotDF[,'Include']>0 )
  if( Rescale!=FALSE ) Mat = Mat / outer(rep(Rescale,nrow(Mat)), colMeans(Mat[Which,]))
  
  # Plotting functions
  f = function(Num, zlim=NULL){
    if( is.null(zlim)) Return = ((Num)-min((Num),na.rm=TRUE))/diff(range((Num),na.rm=TRUE))
    if( !is.null(zlim)) Return = ((Num)-zlim[1])/diff(zlim)
    return(Return)
  }
  Col = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red"))
  
  # Plot
  Par = list(...)
  if(Format=="png"){
    png(file=paste0(FileName, ".png"),
        width=Par$mfrow[2]*MapSizeRatio['Width(in)'],
        height=Par$mfrow[1]*MapSizeRatio['Height(in)'], res=Res, units='in')
  }
  if(Format=="jpg"){
    jpeg(file=paste0(FileName, ".jpg"),
         width=Par$mfrow[2]*MapSizeRatio['Width(in)'],
         height=Par$mfrow[1]*MapSizeRatio['Height(in)'], res=Res, units='in')
  }
  if(Format%in%c("tif","tiff")){
    tiff(file=paste0(FileName, ".tif"),
         width=Par$mfrow[2]*MapSizeRatio['Width(in)'],
         height=Par$mfrow[1]*MapSizeRatio['Height(in)'], res=Res, units='in')
  }
    if( add==FALSE ) par( Par )          # consider changing to Par=list() input, which overloads defaults a la optim() "control" input
    for(t in 1:length(Year_Set)){
      if( is.null(MappingDetails) ){
        plot(1, type="n", ylim=Ylim, xlim=Xlim, main="", xlab="", ylab="")#, main=Year_Set[t])
        points(x=PlotDF[Which,'Lon'], y=PlotDF[Which,'Lat'], col=Col(n=50)[ceiling(f(Mat[Which,],zlim=zlim)[,t]*49)+1], cex=0.01)
      }else{
        boundary_around_limits = 3
        Map = maps::map(MappingDetails[[1]], MappingDetails[[2]], plot=FALSE, ylim=mean(Ylim)+boundary_around_limits*c(-0.5,0.5)*diff(Ylim), xlim=mean(Xlim)+boundary_around_limits*c(-0.5,0.5)*diff(Xlim), fill=TRUE) # , orientation=c(mean(y.lim),mean(x.lim),15)
        Tmp1 = na.omit( cbind('PID'=cumsum(is.na(Map$x)), 'POS'=1:length(Map$x), 'X'=Map$x, 'Y'=Map$y, matrix(0,ncol=length(Year_Set),nrow=length(Map$x),dimnames=list(NULL,Year_Set))) )
        TmpLL = rbind( Tmp1, cbind('PID'=max(Tmp1[,1])+1,'POS'=1:length(Which)+max(Tmp1[,2]),'X'=PlotDF[Which,'Lon'], 'Y'=PlotDF[Which,'Lat'], Mat[Which,]) )
        attr(TmpLL,"projection") = "LL"
        attr(TmpLL,"zone") = zone
        tmpUTM = suppressMessages(PBSmapping::convUL(TmpLL))                                                         #$
        coordinates(tmpUTM) = c("X","Y")
        tmpUTM_rotated <- maptools::elide( tmpUTM, rotate=Rotate)
        plot(tmpUTM_rotated[-c(1:nrow(Tmp1)),], pch="", xlim=range(tmpUTM_rotated@coords[-c(1:nrow(Tmp1)),'x']), ylim=range(tmpUTM_rotated@coords[-c(1:nrow(Tmp1)),'y']) )
        Col_Bin = ceiling(f(tmpUTM_rotated@data[-c(1:nrow(Tmp1)),-c(1:2),drop=FALSE],zlim=zlim)[,t]*49) + 1
        if( any(Col_Bin<1 | Col_Bin>50) ) stop("zlim doesn't span the range of the variable")
        points(x=tmpUTM_rotated@coords[-c(1:nrow(Tmp1)),'x'], y=tmpUTM_rotated@coords[-c(1:nrow(Tmp1)),'y'], col=Col(n=50)[Col_Bin], cex=Cex, pch=pch)
        lev = levels(as.factor(tmpUTM_rotated@data$PID))
        for(levI in 1:(length(lev)-1)) {
          indx = which(tmpUTM$PID == lev[levI])
          if( var(sign(TmpLL[indx,'Y']))==0 ){
            polygon(x=tmpUTM_rotated@coords[indx,'x'], y=tmpUTM_rotated@coords[indx,'y'], col="grey")
          }else{
            warning( "Skipping map polygons that straddle equation, because PBSmapping::convUL doesn't work for these cases" )
          }
        }
      }
      title( Year_Set[t], line=0.1, cex.main=ifelse(is.null(Par$cex.main), 1.8, Par$cex.main), cex=ifelse(is.null(Par$cex.main), 1.8, Par$cex.main) )
      box()
      #if(t==1) compassRose( x=c(0.75,0.25)%*%par()$usr[1:2], y=c(0.25,0.75)%*%par()$usr[3:4], rotate=Rotate)
    }
    mtext(side=1, outer=TRUE, outermargintext[1], cex=1.75, line=par()$oma[1]/2)
    mtext(side=2, outer=TRUE, outermargintext[2], cex=1.75, line=par()$oma[2]/2)
  if(Format %in% c("png","jpg","tif","tiff")) dev.off()
  # Legend
  if(Format=="png"){
    png(file=paste0(FileName, "_Legend.png",sep=""),
        width=1, height=2*MapSizeRatio['Height(in)'], res=Res, units='in')
  }
  if(Format=="jpg"){
    jpeg(file=paste0(FileName, "_Legend.jpg",sep=""),
         width=1, height=2*MapSizeRatio['Height(in)'], res=Res, units='in')
  }
  if(Format%in%c("tif","tiff")){
    tiff(file=paste0(FileName, "_Legend.tif",sep=""),
         width=1, height=2*MapSizeRatio['Height(in)'], res=Res, units='in')
  }
  if(Format %in% c("png","jpg","tif","tiff")){
    SpatialDeltaGLMM:::Heatmap_Legend( colvec=Col(n=50), heatrange=range(Mat), textmargin=textmargin )
    dev.off()
  }
  return( invisible(list("Par"=Par, "par"=par())) )
}
