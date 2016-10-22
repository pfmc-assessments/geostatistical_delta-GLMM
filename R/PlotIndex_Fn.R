
#' @title
#' Plot index of abundance
#'
#' @description
#' \code{PlotIndex_Fn} plots an index proportion to population abundance
#'
#' @param TmbData Formatted data inputs, from `SpatialDeltaGLMM::Data_Fn(...)`
#' @param DirName Directory for saving plot and table
#' @param PlotName Name for plot
#' @param interval_width width for confidence intervals
#' @param strata_names names for spatial strata
#' @param category_names names for categories (if using package `VAST`)
#' @param use_biascorr Boolean, whether to use bias-corrected estimates if available
#' @param plot_legend Add legend for labelling colors
#' @param total_area_km2 Total area for calculating a design-based estimator using one design-stratum (only recommended for model exploration)
#' @param plot_log Boolean, whether to plot y-axis in log-scale
#' @param width plot width in inches
#' @param height plot height in inches
#' @param ... Other inputs to `par()`
#' @inheritParams PlotResultsOnMap_Fn
#'
#' @return Return Tagged list of output
#' \describe{
#'   \item{Table}{table of index estimates by stratum and year, e.g., for including in an assessment model}
#' }
#'

#' @export
PlotIndex_Fn <-
function( TmbData, Sdreport, Year_Set=NULL, Years2Include=NULL, DirName=paste0(getwd(),"/"), PlotName="Index.png", interval_width=1,
  strata_names=NULL, category_names=NULL, use_biascorr=FALSE, plot_legend=TRUE, total_area_km2=NULL, plot_log=FALSE, width=4, height=4, ... ){

  # Which parameters
  if( "ln_Index_tl" %in% rownames(TMB:::summary.sdreport(Sdreport)) ){
    ParName = "Index_tl"
    TmbData[['n_c']] = 1
  }
  if( "ln_Index_ctl" %in% rownames(TMB:::summary.sdreport(Sdreport)) ){
    ParName = "Index_ctl"
  }
  if( "Index_tp" %in% rownames(TMB:::summary.sdreport(Sdreport)) ){
    ParName = "Index_tp"
    TmbData[["n_l"]] = 1
    TmbData[["n_c"]] = TmbData[["n_p"]]
  }

  # Fill in missing
  if( is.null(Year_Set) ) Year_Set = 1:TmbData$n_t
  if( is.null(Years2Include) ) Years2Include = 1:TmbData$n_t
  if( is.null(strata_names) ) strata_names = 1:TmbData$n_l
  if( is.null(category_names) ) category_names = 1:TmbData$n_c

  # Extract index
  if( ParName %in% c("Index_tl","Index_ctl")){
    if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
      log_Index_ctl = array( c(Sdreport$unbiased$value[which(names(Sdreport$value)==paste0("ln_",ParName))],TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==paste0("ln_",ParName)),'Std. Error']), dim=c(unlist(TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
      Index_ctl = array( c(Sdreport$unbiased$value[which(names(Sdreport$value)==ParName)],TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==ParName),'Std. Error']), dim=c(unlist(TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
    }else{
      log_Index_ctl = array( TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==paste0("ln_",ParName)),], dim=c(unlist(TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
      Index_ctl = array( TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==ParName),], dim=c(unlist(TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
    }
  }
  if( ParName %in% c("Index_tp")){
    if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
      Index_ctl = aperm( array( c(Sdreport$unbiased$value[which(names(Sdreport$value)==ParName)],TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==ParName),'Std. Error']), dim=c(unlist(TmbData[c('n_t','n_c','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) ), perm=c(2,1,3))
      if( "ln_Index_tp" %in% rownames(TMB:::summary.sdreport(Sdreport))){
        log_Index_ctl = aperm( array( c(Sdreport$unbiased$value[which(names(Sdreport$value)==paste0("ln_",ParName))],TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==paste0("ln_",ParName)),'Std. Error']), dim=c(unlist(TmbData[c('n_t','n_c','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) ), perm=c(2,1,3))
      }else{
        log_Index_ctl = log( Index_ctl )
        log_Index_ctl[,,,'Std. Error'] = log_Index_ctl[,,,'Std. Error'] / log_Index_ctl[,,,'Estimate']
        warning( "Using kludge for log-standard errors of index, to be replaced in later versions of 'SpatialVAM'" )
      }
    }else{
      Index_ctl = aperm( array( TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==ParName),], dim=c(unlist(TmbData[c('n_t','n_c','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) ), perm=c(2,1,3,4))
      if( "ln_Index_tp" %in% rownames(TMB:::summary.sdreport(Sdreport))){
       log_Index_ctl = aperm( array( TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))==paste0("ln_",ParName)),], dim=c(unlist(TmbData[c('n_t','n_c','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) ), perm=c(2,1,3,4))
      }else{
        log_Index_ctl = log( Index_ctl )
        log_Index_ctl[,,,'Std. Error'] = log_Index_ctl[,,,'Std. Error'] / log_Index_ctl[,,,'Estimate']
        warning( "Using kludge for log-standard errors of index, to be replaced in later versions of 'SpatialVAM'" )
      }
    }
  }

  # Calculate design-based
  if( !is.null(total_area_km2) & TmbData$n_c==1 ){
    message( "Calculating naive design-based index -- do not use this, its intended only for comparison purposes" )
    Calc_design = TRUE
    Design_t = tapply( TmbData$b_i/TmbData$a_i, INDEX=TmbData$t_i, FUN=mean ) * total_area_km2 / 1000 # Convert to tonnes
    Design_t = cbind( "Estimate"=Design_t, "Std. Error"=sqrt(tapply(TmbData$b_i/TmbData$a_i,INDEX=TmbData$t_i,FUN=var)/tapply(TmbData$b_i/TmbData$a_i,INDEX=TmbData$t_i,FUN=length))*total_area_km2/1000)
    Design_t = cbind( Design_t, "CV"=Design_t[,'Std. Error'] / Design_t[,'Estimate'] )
  }else{
    Calc_design = FALSE
  }
  
  # Plot
  Par = list( mar=c(2,2,1,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", oma=c(1,1,0,0), mfrow=c(ceiling(sqrt(TmbData$n_c)),ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))), ... )
  png( file=paste0(DirName,"/",PlotName), width=width, height=height, res=200, units="in")
    par( Par )
    for( cI in 1:TmbData$n_c ){
      # Calculate y-axis limits
      Ylim = c(0, max(Index_ctl[cI,Years2Include,,'Estimate']%o%c(1,1) * exp(log_Index_ctl[cI,Years2Include,,'Std. Error']%o%c(-interval_width,interval_width))) )
      if( plot_log==TRUE ) Ylim[1] = min(Index_ctl[cI,Years2Include,,'Estimate']%o%c(1,1) * exp(log_Index_ctl[cI,Years2Include,,'Std. Error']%o%c(-interval_width,interval_width)))
      if( Calc_design==TRUE ) Ylim[2] = max(Ylim[2], (Design_t[,'Estimate']%o%c(1,1))+Design_t[,'Std. Error']%o%c(-interval_width,interval_width))
      if( Calc_design==TRUE & plot_log==TRUE ) Ylim[2] = min(Ylim[2], (Design_t[,'Estimate']%o%c(1,1))+Design_t[,'Std. Error']%o%c(-interval_width,interval_width))
      # Plot stuff
      plot(1, type="n", xlim=range(Year_Set), ylim=ifelse(plot_legend==TRUE,1.25,1.05)*Ylim, xlab="", ylab="", main=ifelse(TmbData$n_c>1,category_names[cI],""), log=ifelse(plot_log==TRUE,"y","") )
      for(l in 1:TmbData$n_l){
        SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Index_ctl[cI,Years2Include,l,'Estimate'], x=Year_Set[Years2Include]+seq(-0.1,0.1,length=TmbData$n_l)[l], ybounds=(Index_ctl[cI,Years2Include,l,'Estimate']%o%c(1,1))*exp(log_Index_ctl[cI,Years2Include,l,'Std. Error']%o%c(-interval_width,interval_width)), type="b", col=rainbow(TmbData[['n_l']])[l], col_bounds=rainbow(TmbData[['n_l']])[l], ylim=Ylim)
        if(Calc_design==TRUE) SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Design_t[,'Estimate'], x=Year_Set[Years2Include]+seq(-0.1,0.1,length=TmbData$n_l)[l], ybounds=(Design_t[,'Estimate']%o%c(1,1))+Design_t[,'Std. Error']%o%c(-interval_width,interval_width), type="b", col="black", col_bounds="black")
      }
      if(plot_legend==TRUE) legend( "top", bty="n", fill=c(na.omit(ifelse(Calc_design==TRUE,"black",NA)),rainbow(TmbData[['n_l']])), legend=c(na.omit(ifelse(Calc_design==TRUE,"Design-based",NA)),as.character(strata_names)), ncol=2 )
    }
    mtext( side=1:2, text=c("Year","Abundance (metric tonnes)"), outer=TRUE, line=c(0,0) )
  dev.off()

  # Write to file
  Table = NULL
  for( cI in 1:TmbData$n_c ){
    Tmp = data.frame( "Year"=Year_Set, "Unit"=1, "Fleet"=rep(strata_names,each=TmbData$n_t), "Estimate_metric_tons"=as.vector(Index_ctl[cI,,,'Estimate']), "SD_log"=as.vector(log_Index_ctl[cI,,,'Std. Error']), "SD_mt"=as.vector(Index_ctl[cI,,,'Std. Error']) )
    if( TmbData$n_c>1 ) Tmp = cbind( "Category"=category_names[cI], Tmp)
    Table = rbind( Table, Tmp )
  }
  if(!is.null(total_area_km2)) Table = cbind(Table, "Naive_design-based_index"=Design_t)
  write.csv( Table, file=paste0(DirName,"/Table_for_SS3.csv"), row.names=FALSE)

  # Return stuff
  Return = list( "Table"=Table, "log_Index_ctl"=log_Index_ctl, "Index_ctl"=Index_ctl, "Ylim"=Ylim)
  return( invisible(Return) )
}
