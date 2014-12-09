PlotAniso_Fn <-
function( FileName, Report, ControlList=list("Width"=5, "Height"=5, "Res"=200, "Units"='in')){
  png(file=FileName, width=ControlList$Width, height=ControlList$Height, res=ControlList$Res, units=ControlList$Units)
    par( mar=c(2,2,0,0), mgp=c(1.5,0.5,0), tck=-0.02)
    Eigen = eigen(Report$H)
    plot( 1, type="n", xlim=c(-1,1)*max(Eigen$values), ylim=c(-1,1)*max(Eigen$values))
    arrows( x0=rep(0,2), y0=rep(0,2), x1=Eigen$vectors[1,]*Eigen$values, y1=Eigen$vectors[2,]*Eigen$values)
  dev.off()
}
