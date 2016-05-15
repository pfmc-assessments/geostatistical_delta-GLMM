

Plot_Ellipse_Fn = function( filename, yearset=NULL, years2include=NULL, MappingDetails, MapSizeRatio=c("Height(in)"=4,"Width(in)"=4), Xlim, Ylim, ZinUTM=TRUE, zone=NA, ncol_legend=2, Format="png", ... ){
  # Only run if necessary outputs are available
  if( all(c("mean_Z_tl","cov_Z_tl") %in% names(Report)) ){
    # Decide on years
    if( is.null(years2include) ) years2include = 1:nrow(Report$mean_Z_tl)

    # Plot ellipse -- Lat/Lon OR Lat/Depth
    if(Format=="png") png( file=filename, width=MapSizeRatio['Width(in)'], height=MapSizeRatio['Height(in)'], res=200, units="in")
      # Settings
      ColBorder = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red","darkred"))(nrow(Report$mean_Z_tl)) # c("blue","red")# c("darkblue","blue","lightblue","lightgreen","yellow","orange","red","darkred")
      if( ZinUTM==FALSE ){
        maps::map(MappingDetails[[1]], MappingDetails[[2]], ylim=mean(Ylim)+1*c(-0.5,0.5)*diff(Ylim), xlim=mean(Xlim)+1*c(-0.5,0.5)*diff(Xlim), fill=TRUE, ...) # , orientation=c(mean(y.lim),mean(x.lim),15)
      }else{
        SpatialDeltaGLMM:::Plot_States_in_UTM_Fn( MappingDetails=MappingDetails, fillcol=NA, xlim=Xlim, ylim=Ylim, zone=zone, ... )
      }
      # Plot ellipsoids
      for(t in years2include){
        Shape = mixtools::ellipse(mu=Report$mean_Z_tl[t,1:2], sigma=Report$cov_Z_tl[t,1:2,1:2], alpha=1-(1-pnorm(1))*2, newplot=FALSE, draw=FALSE) #, type="l")
        polygon( Shape, border=ColBorder[t], lwd=3 )    # , col=Col[t]
      }
      if( !is.null(yearset)) legend( "top", fill=ColBorder[years2include], bty="n", legend=yearset[years2include], ncol=ncol_legend)
      #for(t in 2:length(Col)) arrows( x0=mean_Z_tl[t-1,1], y0=mean_Z_tl[t-1,2], x1=mean_Z_tl[t,1], y1=mean_Z_tl[t,2], length=0.1 )
    if(Format=="png") dev.off()
  }else{
    message("mean_Z_tl or cov_Z_tl not found in Report")
  }
}
