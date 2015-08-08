PlotCov_Fn <-
function(Report, NN_Extrap, Cov_iz, FileName, ControlList=list("Width"=5*3, "Height"=2*3, "Res"=200, "Units"='in')){ 
  # Extract outputs
  D_it = Report$D_xt[NN_Extrap$nn.idx,]
  R1_it = Report$R1_xt[NN_Extrap$nn.idx,]
  R2_it = Report$R2_xt[NN_Extrap$nn.idx,]
  for(CovI in 1:ncol(Cov_iz)){
    png(file=paste0(FileName,colnames(Cov_iz)[CovI],".png"), width=ControlList$Width, height=ControlList$Height, res=ControlList$Res, units=ControlList$Units)
      par(mfrow=c(2,6), oma=c(0,0,2,0), mar=c(2,2,0,0), mgp=c(1.5,0.5,0), tck=-0.02)
      for(t in 1:length(Year_Set)){
        plot(x=Cov_iz[,CovI], y=R2_it[,t], type="p", col="red", ylim=c(0,max(R2_it)))
        points(x=Cov_iz[,CovI], y=R1_it[,t]*max(R2_it), type="p", col="blue")
        points(x=Cov_iz[,CovI], y=D_it[,t], type="p", col="black")
      }
      mtext( side=3, outer=TRUE, colnames(X_xj)[CovI] )
    dev.off()
  }
}
