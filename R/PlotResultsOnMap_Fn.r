#' @title
#' Plot standard maps
#'
#' @description
#' \code{PlotResultsOnMap_Fn} plots a standard set of diagnostic maps
#'
#' @param plot_set integer-vector defining plots to create
#' \describe{
#'   \item{plot_set=1}{Probability of encounter/non-encounter}
#'   \item{plot_set=2}{Log-expected positive catch rate}
#'   \item{plot_set=3}{Log-predicted density (product of encounter probability and positive catch rates)}
#'   \item{plot_set=4}{Log-pPositive catch rates (rescaled)}
#'   \item{plot_set=5}{Log-predicted density (rescaled)}
#'   \item{plot_set=6}{Spatio-temporal variation in encounter probability}
#'   \item{plot_set=7}{Spatio-temporal variation in log-positive catch rates}
#'   \item{plot_set=8}{Linear predictor for encounter probability}
#'   \item{plot_set=9}{Linear predictor for positive catch rates}
#'   \item{plot_set=10}{Coefficient of variation for predicted density (available only if \code{Data_Fn(...,Options=c('SD_site_logdensity'=1,...))}}
#' }
#' @param MappingDetails tagged list of plot-settings from \code{MapDetails_Fn}
#' @param Report tagged list of outputs from TMB model via \code{Obj$report()}
#' @param Sdreport Standard deviation outputs from TMB model via \code{sdreport(Obj)}
#' @param Nknots Number of knots used for plotting (default is all grid cells in \code{Extrapolation_grid}
#' @param MapSizeRatio Default size for each panel
#' @param Ylim ylimits for each panel
#' @param Xlim xlimits for each panel
#' @param FileName Directory (absolute path) and base for filenames of plots
#' @param Year_Set Year names for labeling panels
#' @param Years2Include integer vector, specifying positions of \code{Year_Set} for plotting (used to avoid plotting years with no data, etc.)
#' @param category_names character vector specifying names for different categories (only used for R package \code{VAST})
#' @param Legend tagged list specifying insert colorbar
#' \describe{
#'   \item{use}{Boolean whether to plot insert colorbar or not}
#'   \item{x}{Left and right-hand limits for legend in percentage of panel}
#'   \item{y}{bottom and top limits for legend in percentage of panel}
#' }
#'
#' @return Mat_xt a matrix (rows: modeled knots; column: modeled year) for plotted output of last element of \code{plot_set}
#'

#' @export
PlotResultsOnMap_Fn <-
function(plot_set=1:5, MappingDetails, Report, Sdreport=NULL, Nknots=Inf, PlotDF, MapSizeRatio=c('Width(in)'=4,'Height(in)'=4), Xlim, Ylim, FileName=paste0(getwd(),"/"),
         Year_Set=NULL, Years2Include=NULL, Rescale=FALSE, Rotate=0, Format="png", Res=200, zone=NA, Cex=0.01, add=FALSE, category_names=NULL,
         textmargin=NULL, pch=NULL, Legend=list("use"=FALSE, "x"=c(10,30), "y"=c(10,30)), mfrow=NULL, plot_legend_fig=TRUE, ...){

  # Fill in missing inputs
  if( "D_xt" %in% names(Report)){
    if( is.null(Year_Set) ) Year_Set = 1:ncol(Report$D_xt)
    if( is.null(Years2Include) ) Years2Include = 1:ncol(Report$D_xt)
    category_names = "singlespecies"
    Ncategories = length(category_names)
  }
  if( "D_xct" %in% names(Report)){
    if( is.null(Year_Set) ) Year_Set = 1:dim(Report$D_xct)[3]
    if( is.null(Years2Include) ) Years2Include = 1:dim(Report$D_xct)[3]
    if( is.null(category_names) ) category_names = 1:dim(Report$D_xct)[2]
    Ncategories = dim(Report$D_xct)[2]
  }
  if( "dhat_ktp" %in% names(Report)){
    if( is.null(Year_Set) ) Year_Set = 1:dim(Report$dhat_ktp)[2]
    if( is.null(Years2Include) ) Years2Include = 1:dim(Report$dhat_ktp)[2]
    if( is.null(category_names) ) category_names = 1:dim(Report$dhat_ktp)[3]
    Ncategories = dim(Report$dhat_ktp)[3]
  }
  if( is.null(mfrow)) mfrow = c(ceiling(sqrt(length(Years2Include))), ceiling(length(Years2Include)/ceiling(sqrt(length(Years2Include)))))

  # Extract elements
  plot_codes <- c("Pres", "Pos", "Dens", "Pos_Rescaled", "Dens_Rescaled", "Eps_Pres", "Eps_Pos", "LinPred_Pres", "LinPred_Pos", "Dens_CV")
  if( is.null(textmargin)){
    textmargin <- c("Probability of encounter", "Density, ln(kg. per square km.)", "Density, ln(kg. per square km.)", "", "", "", "", "", "", "CV of density (dimensionless)")
  }

  # Select locations to plot
  if( Nknots<Inf ){
    NN_plot = RANN::kmeans(x=PlotDF[,c("Lon","Lat")], centers=Nknots, iter.max=50, nstart=2, trace=0)
    Match = match( 1:Nknots, NN_plot$cluster)
    PlotDF = PlotDF[Match,]
    message( "Restricted plotting locations to ", Nknots, " locations" )
  }

  # Loop through plots
  for( cI in 1:Ncategories){
    for(plot_num in plot_set){
      if(plot_num==1){
        # Presence/absence ("Pres")
        if("D_xt"%in%names(Report)) Mat_xt = Report$R1_xt
        if("D_xct"%in%names(Report)) Mat_xt = Report$R1_xct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==2){
        # Positive values ("Pos")
        if("D_xt"%in%names(Report)) Mat_xt = log(Report$R2_xt)
        if("D_xct"%in%names(Report)) Mat_xt = log(Report$R2_xct)[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==3){
        # Density ("Dens")
        if("D_xt"%in%names(Report)) Mat_xt = log(Report$D_xt)
        if("D_xct"%in%names(Report)) Mat_xt = log(Report$D_xct)[,cI,]
        if("dhat_ktp"%in%names(Report)) Mat_xt = Report$dhat_ktp[,,cI]
      }
      if(plot_num==4){
        # Positive values rescaled ("Pos_Rescaled")
        if("D_xt"%in%names(Report)) Mat_xt = log(Report$R2_xt+quantile(Report$R2_xt,0.25))
        if("D_xct"%in%names(Report)) Mat_xt = log(Report$R2_xct+quantile(Report$R2_xct,0.25))[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==5){
        # Density rescaled ("Dens_Rescaled")
        if("D_xt"%in%names(Report)) Mat_xt = log(Report$D_xt+quantile(Report$D_xt,0.25))
        if("D_xct"%in%names(Report)) Mat_xt = log(Report$D_xct+quantile(Report$D_xct,0.25))[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==6){
        # Epsilon for presence/absence ("Eps_Pres")
        if("D_xt"%in%names(Report)) Mat_xt = Report$Epsilon1_st
        if("D_xct"%in%names(Report)) Mat_xt = Report$Epsilon1_sct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==7){
        # Epsilon for positive values ("Eps_Pos")
        if("D_xt"%in%names(Report)) Mat_xt = Report$Epsilon2_st
        if("D_xct"%in%names(Report)) Mat_xt = Report$Epsilon2_sct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==8){
        # Linear predictor for probability of encounter
        if("D_xt"%in%names(Report)) Mat_xt = Report$P1_xt
        if("D_xct"%in%names(Report)) Mat_xt = Report$P1_xct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==9){
        # Linear predictor for positive catch rates
        if("D_xt"%in%names(Report)) Mat_xt = Report$P2_xt
        if("D_xct"%in%names(Report)) Mat_xt = Report$P2_xct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==10){
        # Density ("Dens") CV             # Index_xtl
        if( is.null(Sdreport) ) stop("Must supply 'Sdreport' if 'plot_num=10'")
        if("D_xt"%in%names(Report)){
          if( !("log(Index_xtl)" %in% rownames(TMB::summary.sdreport(Sdreport))) ) stop("Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'SpatialDeltaGLMM'")
          Mat_xt = array( TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))=="log(Index_xtl)"),], dim=c(dim(Report$D_xt),ncol(Report$Index_tl),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )[,,1,'Std. Error']
        }
        if("D_xct"%in%names(Report)){
          if( !("log(Index_xctl)" %in% rownames(TMB::summary.sdreport(Sdreport))) ) stop("Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'VAST'")
          Mat_xt = array( TMB:::summary.sdreport(Sdreport)[which(rownames(TMB:::summary.sdreport(Sdreport))=="log(Index_xctl)"),], dim=c(dim(Report$D_xct),dim(Report$Index_ctl)[3],2), dimnames=list(NULL,NULL,NULL,NULL,c('Estimate','Std. Error')) )[,cI,,1,'Std. Error']
        }
        if("dhat_ktp"%in%names(Report)) stop("'plot_num=10' not implemented for 'SpatialVAM'")
        # Convert to CV
        Mat_xt = sqrt( exp(Mat_xt^2) - 1 )
      }
      # Do plot    #
      if(add==FALSE) par( mfrow=mfrow )
      Return = PlotMap_Fn( MappingDetails=MappingDetails, Mat=Mat_xt[,Years2Include], PlotDF=PlotDF, MapSizeRatio=MapSizeRatio, Xlim=Xlim, Ylim=Ylim, FileName=paste0(FileName,plot_codes[plot_num],ifelse(Ncategories>1,paste0("--",category_names[cI]),"")), Year_Set=Year_Set[Years2Include], Rescale=Rescale, Rotate=Rotate, Format=Format, Res=Res, zone=zone, Cex=Cex, textmargin=textmargin[plot_num], add=add, pch=pch, Legend=Legend, mfrow=mfrow, plot_legend_fig=plot_legend_fig, ...)
    }          # SpatialDeltaGLMM:::
  }
  return( invisible(Mat_xt) )
}
