PlotIndex_Fn <-
function( PlotName="Index.png", DirName, TmbData, Sdreport, Year_Set=NULL, Years2Include=NULL, strata.limits, use_biascorr=FALSE, ... ){
  # Fill in missing
  if( is.null(Year_Set) ) Year_Set = 1:TmbData$n_t
  if( is.null(Years2Include) ) Years2Include = 1:TmbData$n_t
  
  # Extract index
  if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
    # Must add!
  }else{
    log_Index = array( summary(Sdreport)[which(rownames(summary(Sdreport))=="ln_Index_tl"),], dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
    Index = array( summary(Sdreport)[which(rownames(summary(Sdreport))=="Index_tl"),], dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
  }
  
  # Plot
  png( file=paste0(DirName,"/",PlotName), width=4, height=4, res=200, units="in")
    par( mar=c(3,3,2,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", ...)
    plot(1, type="n", xlim=range(Year_Set), ylim=1.05*c(0,max(exp(log_Index[Years2Include,,'Estimate']+1*log_Index[Years2Include,,'Std. Error']))), xlab="Year", ylab="Abundance (metric tonnes)" )
    for(l in 1:dim(Index)[2]){
      Plot_Points_and_Bounds_Fn( y=Index[Years2Include,l,'Estimate'], x=Year_Set[Years2Include]+seq(-0.1,0.1,length=dim(Index)[2])[l], ybounds=exp(log_Index[Years2Include,l,'Estimate']%o%c(1,1)+log_Index[Years2Include,l,'Std. Error']%o%c(-1,1)), type="b", col=rainbow(TmbData[['n_l']])[l], col_bounds=rainbow(TmbData[['n_l']])[l]) 
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
