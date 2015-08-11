PlotIndex_Fn <-
function( PlotName="Index.png", DirName, TmbData, Sdreport, Year_Set, strata.limits, ... ){
  png( file=paste0(DirName,"/",PlotName), width=4, height=4, res=200, units="in")
    par( mar=c(3,3,2,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", ...)
    log_Index = array( summary(Sdreport)[which(rownames(summary(Sdreport))=="ln_Index_tl"),], dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
    Index = array( summary(Sdreport)[which(rownames(summary(Sdreport))=="Index_tl"),], dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
    plot(1, type="n", xlim=range(Year_Set), ylim=1.05*c(0,max(exp(log_Index[,,'Estimate']+1*log_Index[,,'Std. Error']))), xlab="Year", ylab="Abundance (metric tonnes)" )
    for(l in 1:dim(Index)[2]){
      lines( y=Index[,l,'Estimate'], x=Year_Set+seq(-0.1,0.1,length=dim(Index)[2])[l], type="b", col=rainbow(TmbData[['n_l']])[l] )
      for(t in 1:dim(Index)[1]){
        lines( x=rep(Year_Set[t],2)+seq(-0.1,0.1,length=dim(Index)[2])[l], y=exp(log_Index[t,l,'Estimate']+c(-1,1)*log_Index[t,l,'Std. Error']), col=rainbow(TmbData[['n_l']])[l])
      }
    }
    # Write to file
    Names = strata.limits[,'STRATA']
    Table = data.frame( "Year"=Year_Set, "Unit"=1, "Fleet"=rep(Names,each=dim(Index)[1]), "Estimate (metric tonnes)"=as.vector(Index[,,'Estimate']), "SD"=as.vector(log_Index[,,'Std. Error']) )
    write.csv( Table, file=paste0(DirName,"/Table_for_SS3.csv"), row.names=FALSE)
  dev.off()

  # Return stuff
  Return = list( "Table"=Table, "log_Index"=log_Index, "Index"=Index)
  return( Return )
}
