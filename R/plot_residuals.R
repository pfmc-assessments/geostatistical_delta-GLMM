#' @title
#' Plot Pearson residuals on map
#'
#' @description
#' \code{plot_residuals} shows average Pearson residual for every knot for encounter probability and positive catch rate components
#'
#' @param Lat_i Latitude for every observation \code{i}
#' @param Lon_i Longitude for every observation \code{i}
#' @param Q Output from \code{QQ_Fn}
#' @param savdir directory to use when saving results
#' @inheritParams Build_TMB_Fn
#' @inheritParams PlotResultsOnMap_Fn
#' @param ... arguments passed to \code{PlotMap_Fn}
#'
#' @return A tagged list of Pearson residuals
#' \describe{
#'   \item{Q1_xt}{Matrix of average residuals for encounter/non-encounter component by site \code{x} and year \code{t}}
#'   \item{Q2_xt}{Matrix of average residuals for positive-catch-rate component by site \code{x} and year \code{t}}
#' }

#' @export
plot_residuals = function( Lat_i, Lon_i, TmbData, Report, Q, savedir=getwd(),
         MappingDetails, PlotDF, MapSizeRatio=c('Width(in)'=4,'Height(in)'=4), Xlim, Ylim,
         FileName=paste0(getwd(),"/"), Year_Set=NULL, Years2Include=NULL, Rescale=FALSE, Rotate=0, Format="png", Res=200,
         zone=NA, Cex=0.01, add=FALSE, textmargin=NULL, pch=NULL,
         Legend=list("use"=FALSE, "x"=c(10,30), "y"=c(10,30)), mfrow=NULL, plot_legend_fig=TRUE, ... ){

  ##################
  # Basic inputs
  ##################

  if( is.null(Year_Set) ) Year_Set = 1:TmbData$n_t
  if( is.null(Years2Include) ) Years2Include = 1:TmbData$n_t
  if( is.null(mfrow)) mfrow = c(ceiling(sqrt(length(Years2Include))), ceiling(length(Years2Include)/ceiling(sqrt(length(Years2Include)))))

  ##################
  # Presence-absence
  # http://data.princeton.edu/wws509/notes/c3s8.html
  ##################

  # Extract binomial stuff for encounter-nonencounter data
  exp_rate_xt = tapply( Report$R1_i, INDEX=list(TmbData$s_i,factor(TmbData$t_i,levels=1:TmbData$n_t-1)), FUN=mean )
  obs_rate_xt = tapply( TmbData$b_i>0, INDEX=list(TmbData$s_i,factor(TmbData$t_i,levels=1:TmbData$n_t-1)), FUN=mean )
  total_num_xt = tapply( TmbData$b_i>0, INDEX=list(TmbData$s_i,factor(TmbData$t_i,levels=1:TmbData$n_t-1)), FUN=length )
  exp_num_xt = exp_rate_xt * total_num_xt
  obs_num_xt = obs_rate_xt * total_num_xt

  # Method #1 -- Binomial cumulative function
  #Q1_xt = pbinom( obs_num_xt, size=total_num_xt, prob=exp_rate_xt )

  # Method #2 -- Pearson residuals
  Q1_xt = (obs_num_xt - exp_num_xt) / sqrt(  exp_num_xt*(total_num_xt-exp_num_xt)/total_num_xt )

  # Method #3 -- Deviance residuals
  #Q1_xt = 2*(obs_num_xt*log(obs_num_xt/exp_num_xt) + (total_num_xt-obs_num_xt)*log((total_num_xt-obs_num_xt)/(total_num_xt-exp_num_xt)))
  #Q1_xt = ifelse( obs_num_xt==0, 2*((total_num_xt-obs_num_xt)*log((total_num_xt-obs_num_xt)/(total_num_xt-exp_num_xt))), Q1_xt)
  #Q1_xt = ifelse( (total_num_xt-obs_num_xt)==0, 2*(obs_num_xt*log(obs_num_xt/exp_num_xt)), Q1_xt)

  ##################
  # Positive catch rates
  ##################

  # Extract quantile for positive catch rates
  Q_i = Q[["Q"]]
  which_pos = which(TmbData$b_i>0)
  var_y = Q[["var_y"]]
  pred_y = Q[["pred_y"]]

  ### Method #1 -- chi-squared transformation of cumulative function
  # Convert to Chi-squared distribution
  #Chisq_x = tapply( Q_i, INDEX=TmbData$s_i[which(TmbData$b_i>0)], FUN=function(vec){sum(-2*log(vec))} )
  # Calculate d.f. for Chi-squared distribution
  #DF_x = 2*tapply( Q_i, INDEX=TmbData$s_i[which(TmbData$b_i>0)], FUN=length )
  # Convert back to uniform distribution
  #P_x = pchisq( q=Chisq_x, df=DF_x )

  ### Method #2 -- average quantile
  #Q2_x = tapply( Q_i, INDEX=TmbData$s_i[which(TmbData$b_i>0)], FUN=mean )
  #Q2_xt = tapply( Q_i, INDEX=list(TmbData$s_i[which(TmbData$b_i>0)],TmbData$t_i[which(TmbData$b_i>0)]), FUN=mean )

  ### Method #3 -- Pearson residuals
  sum_obs_xt = tapply( TmbData$b_i[which_pos], INDEX=list(factor(TmbData$s_i[which_pos],levels=1:TmbData$n_x-1),factor(TmbData$t_i,levels=1:TmbData$n_t-1)[which_pos]), FUN=sum )
  sum_exp_xt = tapply( pred_y, INDEX=list(factor(TmbData$s_i[which_pos],levels=1:TmbData$n_x-1),factor(TmbData$t_i,levels=1:TmbData$n_t-1)[which_pos]), FUN=sum )
  var_exp_xt = tapply( var_y, INDEX=list(factor(TmbData$s_i[which_pos],levels=1:TmbData$n_x-1),factor(TmbData$t_i,levels=1:TmbData$n_t-1)[which_pos]), FUN=sum )
  Q2_xt = (sum_obs_xt - sum_exp_xt) / sqrt(var_exp_xt)

  #################
  # Plots
  #################

  Col = colorRampPalette(colors=c("blue","white","red"))
  textmargin = "Pearson residual"
  for( zI in 1:2 ){
    Q_xt = list( Q1_xt, Q2_xt )[[zI]]
    zlim = c(-1,1) * ceiling(max(abs(Q_xt),na.rm=TRUE))
    #Q_xt = ifelse( abs(Q_xt)>3, 3*sign(Q_xt), Q_xt )
    SpatialDeltaGLMM:::PlotMap_Fn( MappingDetails=MappingDetails, Mat=Q_xt[,Years2Include], PlotDF=PlotDF, Col=Col, zlim=zlim, ignore.na=TRUE, MapSizeRatio=MapSizeRatio, Xlim=Xlim, Ylim=Ylim, FileName=paste0(savedir,"/",c("maps--encounter_pearson_resid","maps--catchrate_pearson_resid")[zI]), Year_Set=Year_Set[Years2Include], Rescale=Rescale, Rotate=Rotate, Format=Format, Res=Res, zone=zone, Cex=Cex, textmargin=textmargin, add=add, pch=pch, Legend=Legend, mfrow=mfrow, plot_legend_fig=plot_legend_fig, ...)
  }

  #################
  # Returns
  #################

  Return = list( "Q1_xt"=Q1_xt, "Q2_xt"=Q2_xt )
  return( invisible(Return) )
}
