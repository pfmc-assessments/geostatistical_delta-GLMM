
#' @export
PlotAniso_Fn <-
function( FileName, Report, ControlList=list("Width"=4, "Height"=5, "Res"=200, "Units"='in'), type="ellipse", TmbData=list("Options_vec"=c("Aniso"=1)) ){
  if( TmbData$Options_vec['Aniso']!=1 ){
    message("Skipping plot of geometric anisotropy because it has been turned off")
  }else{
    # Decomposition
    Eigen = eigen(Report$H)

    # Arrows
    if( type=="arrow" ){
      png(file=FileName, width=ControlList$Width, height=ControlList$Height, res=ControlList$Res, units=ControlList$Units)
        par( mar=c(2,2,0,0), mgp=c(1.5,0.5,0), tck=-0.02)
        plot( 1, type="n", xlim=c(-1,1)*max(Eigen$values), ylim=c(-1,1)*max(Eigen$values))
        arrows( x0=rep(0,2), y0=rep(0,2), x1=Eigen$vectors[1,]*Eigen$values, y1=Eigen$vectors[2,]*Eigen$values)
      dev.off()
    }

    # Ellipses
    if( type=="ellipse" ){
      rss = function(V) sqrt(sum(V[1]^2+V[2]^2))
      Pos_Major = Eigen$vectors[,1]*Eigen$values[1] * Report$Range_raw1
      Pos_Minor = Eigen$vectors[,2]*Eigen$values[2] * Report$Range_raw1
      Pres_Major = Eigen$vectors[,1]*Eigen$values[1] * Report$Range_raw2
      Pres_Minor = Eigen$vectors[,2]*Eigen$values[2] * Report$Range_raw2
      png(file=FileName, width=ControlList$Width, height=ControlList$Height, res=ControlList$Res, units=ControlList$Units)
        par( mar=c(3,3,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
        Range = 1.1 * c(-1,1) * max(abs( cbind(Pos_Major,Pos_Minor, Pres_Major,Pres_Minor) ))
        plot( 1, type="n", xlim=Range, ylim=c(Range[1],Range[2]*1.2), xlab="", ylab="")
        shape::plotellipse( rx=rss(Pres_Major), ry=rss(Pres_Minor), angle=-1*(atan(Pres_Major[1]/Pres_Major[2])/(2*pi)*360-90), lcol=c("green","black")[1], lty=c("solid","dotted")[1])
        shape::plotellipse( rx=rss(Pos_Major), ry=rss(Pos_Minor), angle=-1*(atan(Pos_Major[1]/Pos_Major[2])/(2*pi)*360-90), lcol="black", lty="solid")
        title( "Distance at 10% correlation" )
        mtext(side=1, outer=FALSE, line=2, text="Eastings (km.)")
        mtext(side=2, outer=FALSE, line=2, text="Northings (km.)")
        legend( "top", legend=c("Encounter probability","Positive catch rates"), fill=c("green","black"), bty="n")
        #abline( h=0, v=0, lty="dotted")
      dev.off()
    }
  }
}
