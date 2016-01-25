
Plot_range_quantiles = function( Data_Extrap, Report, a_xl, NN_Extrap, Year_Set=NULL, Prob_vec=c(0.10,0.5,0.90), FileName_Quantiles=paste0(getwd(),"/Range_boundary.png") ){
  # Default inputs
  if( is.null(Year_Set)) Year_Set = 1:TmbData$n_t

  # Plot range boundaries
  calc_quantile = function(x, w, prob=0.5, doplot=FALSE){
    DF = data.frame(x,w)[order(x),]
    DF = cbind(DF, 'cumsum'=cumsum(DF$w)/sum(w))
    Pred = rep(NA,length(prob))
    for(i in 1:length(prob)){
      Between = which( DF$cumsum>prob[i] )[1] - 1:0
      Pred[i] = DF[Between[1],'x'] + (DF[Between[2],'x']-DF[Between[1],'x'])*(prob[i]-DF[Between[1],'cumsum'])/(DF[Between[2],'cumsum']-DF[Between[1],'cumsum'])
    }
    if(doplot==TRUE){
      plot( x=DF$x, y=DF$cumsum, type="l" )
      abline(v=Pred)
    }
    return( Pred )
  }

  # Extrapolation locations
  Data_Extrap_Range = cbind( Data_Extrap[,c('Lat','Lon','N_km','E_km')], 'Include'=ifelse(Data_Extrap[,'Area_km2']>0, TRUE, FALSE) )
  # Add and rescale density
  for(t in 1:length(Year_Set)){
    Data_Extrap_Range = cbind( Data_Extrap_Range, Report$Index_xtl[NN_Extrap$nn.idx,t,1] * (Data_Extrap[,'Area_km2'] / a_xl[NN_Extrap$nn.idx,1]) )
    colnames( Data_Extrap_Range )[ncol(Data_Extrap_Range)] = paste0("Year_",Year_Set[t])
    Data_Extrap_Range[,paste0("Year_",Year_Set[t])] = ifelse( Data_Extrap[,'Area_km2']==0 & a_xl[NN_Extrap$nn.idx,1]==0, 0, Data_Extrap_Range[,paste0("Year_",Year_Set[t])])
  }

  # Plot
  png( file=FileName_Quantiles, width=6.5, height=6.5, res=200, units="in")
    par( mfcol=c(2,2), mar=c(0,2,2,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(4,0,0,0))
    for(z in 1:4){
      Range_Quantile = array( NA, dim=c(length(Year_Set),3), dimnames=list(NULL,c("min","mid","max")) )
      for(t in 1:nrow(Range_Quantile)) Range_Quantile[t,] = calc_quantile( x=Data_Extrap_Range[,c('Lat','Lon','N_km','E_km')[z]], w=Data_Extrap_Range[,paste0('Year_',Year_Set[t])], prob=c(0.05,0.5,0.95), doplot=FALSE )
      matplot( y=Range_Quantile, x=Year_Set, type="l", col="black", lty="solid", lwd=2, xlab="", ylab="", main=c('Latitude','Longitude','Nothings','Eastings')[z], xaxt="n" )
      if(z %in% c(2,4)) axis(1)
    }
    mtext( side=1, text="Year", outer=TRUE, line=2)
  dev.off()

  # Return
  Return = list( "Data_Extrap_Range"=Data_Extrap_Range )
  return( invisible(Return) )
}
