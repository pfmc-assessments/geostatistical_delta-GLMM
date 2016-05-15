#' @export
PlotIndex_Fn <-
function( PlotName="Index.png", DirName, TmbData, Sdreport, Year_Set=NULL, Years2Include=NULL, interval_width=1, strata_names=NULL, use_biascorr=FALSE, plot_legend=TRUE, total_area_km2=NULL, plot_log=FALSE, ... ){
  # Fill in missing
  if( is.null(Year_Set) ) Year_Set = 1:TmbData$n_t
  if( is.null(Years2Include) ) Years2Include = 1:TmbData$n_t
  if( is.null(strata_names) ) strata_names = 1:TmbData$n_l

  # Extract index
  if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
    log_Index = array( c(Sdreport$unbiased$value[which(names(Sdreport$unbiased$value)=="ln_Index_tl")],summary(Sdreport)[which(rownames(summary(Sdreport))=="ln_Index_tl"),'Std. Error']), dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
    Index = array( c(Sdreport$unbiased$value[which(names(Sdreport$unbiased$value)=="Index_tl")],summary(Sdreport)[which(rownames(summary(Sdreport))=="Index_tl"),'Std. Error']), dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
  }else{
    log_Index = array( summary(Sdreport)[which(rownames(summary(Sdreport))=="ln_Index_tl"),], dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
    Index = array( summary(Sdreport)[which(rownames(summary(Sdreport))=="Index_tl"),], dim=c(unlist(TmbData[c('n_t','n_l')]),2), dimnames=list(NULL,NULL,c('Estimate','Std. Error')) )
  }
  
  # Calculate design-based
  if( !is.null(total_area_km2) ){
    message( "Calculating naive design-based index -- do not use this, its intended only for comparison purposes" )
    Calc_design = TRUE
    Design_t = tapply( TmbData$b_i/TmbData$a_i, INDEX=TmbData$t_i, FUN=mean ) * total_area_km2 / 1000 # Convert to tonnes
    Design_t = cbind( "Estimate"=Design_t, "Std. Error"=sqrt(tapply(TmbData$b_i/TmbData$a_i,INDEX=TmbData$t_i,FUN=var)/tapply(TmbData$b_i/TmbData$a_i,INDEX=TmbData$t_i,FUN=length))*total_area_km2/1000)
    Design_t = cbind( Design_t, "CV"=Design_t[,'Std. Error'] / Design_t[,'Estimate'] )
  }else{
    Calc_design = FALSE
  }
  
  # Plot
  Par = list( mar=c(3,3,2,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", ... )
  png( file=paste0(DirName,"/",PlotName), width=4, height=4, res=200, units="in")
    par( Par )
    Ylim = c(0, max(Index[Years2Include,,'Estimate']%o%c(1,1) * exp(log_Index[Years2Include,,'Std. Error']%o%c(-interval_width,interval_width))) )
    if( plot_log==TRUE ) Ylim[1] = min(Index[Years2Include,,'Estimate']%o%c(1,1) * exp(log_Index[Years2Include,,'Std. Error']%o%c(-interval_width,interval_width)))
    if( Calc_design==TRUE ) Ylim[2] = max(Ylim[2], (Design_t[,'Estimate']%o%c(1,1))+Design_t[,'Std. Error']%o%c(-interval_width,interval_width)) 
    if( Calc_design==TRUE & plot_log==TRUE ) Ylim[2] = min(Ylim[2], (Design_t[,'Estimate']%o%c(1,1))+Design_t[,'Std. Error']%o%c(-interval_width,interval_width))
    plot(1, type="n", xlim=range(Year_Set), ylim=ifelse(plot_legend==TRUE,1.25,1.05)*Ylim, xlab="Year", ylab="Abundance (metric tonnes)", log=ifelse(plot_log==TRUE,"y","") )
    for(l in 1:dim(Index)[2]){
      Plot_Points_and_Bounds_Fn( y=Index[Years2Include,l,'Estimate'], x=Year_Set[Years2Include]+seq(-0.1,0.1,length=dim(Index)[2])[l], ybounds=(Index[Years2Include,l,'Estimate']%o%c(1,1))*exp(log_Index[Years2Include,l,'Std. Error']%o%c(-interval_width,interval_width)), type="b", col=rainbow(TmbData[['n_l']])[l], col_bounds=rainbow(TmbData[['n_l']])[l], ylim=Ylim) 
      if(Calc_design==TRUE) Plot_Points_and_Bounds_Fn( y=Design_t[,'Estimate'], x=Year_Set[Years2Include]+seq(-0.1,0.1,length=dim(Index)[2])[l], ybounds=(Design_t[,'Estimate']%o%c(1,1))+Design_t[,'Std. Error']%o%c(-interval_width,interval_width), type="b", col="black", col_bounds="black") 
    }
    if(plot_legend==TRUE) legend( "top", bty="n", fill=c(na.omit(ifelse(Calc_design==TRUE,"black",NA)),rainbow(TmbData[['n_l']])), legend=c(na.omit(ifelse(Calc_design==TRUE,"Design-based",NA)),as.character(strata_names)), ncol=2 )
    # Write to file
    Table = data.frame( "Year"=Year_Set, "Unit"=1, "Fleet"=rep(strata_names,each=dim(Index)[1]), "Estimate (metric tonnes)"=as.vector(Index[,,'Estimate']), "SD (log)"=as.vector(log_Index[,,'Std. Error']), "SD (natural)"=as.vector(Index[,,'Std. Error']) )
    if(!is.null(total_area_km2)) Table = cbind(Table, "Naive_design-based_index"=Design_t)
    write.csv( Table, file=paste0(DirName,"/Table_for_SS3.csv"), row.names=FALSE)
  dev.off()

  # Return stuff
  Return = list( "Table"=Table, "log_Index"=log_Index, "Index"=Index, "Ylim"=Ylim)
  return( invisible(Return) )
}
