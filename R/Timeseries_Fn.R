Timeseries_Fn <-
function(Report, FileName, Year_Set, ControlList=list("Width"=4*3, "Height"=2*3, "Res"=200, "Units"='in')){
  # Extract stuff from report
  D_it = Report$D_xt[NN_Extrap$nn.idx,]
  R1_it = Report$R1_xt[NN_Extrap$nn.idx,]
  R2_it = Report$R2_xt[NN_Extrap$nn.idx,]
  D_xt = Report$D_xt
  # Plot time series
  png(file=FileName, width=ControlList$Width, height=ControlList$Height, res=ControlList$Res, units=ControlList$Units)
    par(mfrow=c(2,4), oma=c(0,0,0,0), mar=c(3,3,2,0), mgp=c(1.5,0.5,0), tck=-0.02)    
    # Entropy
    EntropyFn = function(Vec){ sum(Vec * log(Vec+1e-250)/log(length(Vec)) ) }
    Entropy_t = apply( Report$D_xt, MARGIN=2, FUN=EntropyFn )
    plot( x=Year_Set, y=Entropy_t, type="l", main="Entropy")
    # Variance
    Var_t = apply( Report$D_xt, MARGIN=2, FUN=var )
    plot( x=Year_Set, y=Var_t, type="l", main="Variance", ylim=c(0,max(Var_t)) )
    # CV
    CV_t = apply( Report$D_xt, MARGIN=2, FUN=function(Vec){ sd(Vec)/mean(Vec) } )
    plot( x=Year_Set, y=CV_t, type="l", main="CV", ylim=c(0,max(CV_t)) )
    # Occupancy probability
    Occup_t = colMeans( R1_it )
    plot( x=Year_Set, y=Occup_t, type="l", main="Occup_t", ylim=c(0,1) )    
    # Density
    CondDens_t = colMeans( R2_it )
    plot( x=Year_Set, y=CondDens_t, type="l", main="CondDens_t", ylim=c(0,max(CondDens_t)) )    
    # Density
    Index_t = colMeans( D_it )
    plot( x=Year_Set, y=Index_t, type="l", main="Index_t", ylim=c(0,max(Index_t)) )    
    # correlation between occupancy and abundance
    Cor_t = sapply(1:length(Year_Set), FUN=function(Num){cor(R1_it[,Num],R2_it[,Num],method="spearman")}) 
    plot( x=Year_Set, y=Cor_t, type="l", main="Cor_t", ylim=c(-1,1) ); abline(h=0, lty="dotted")      
  dev.off()
}
