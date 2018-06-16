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

  # Add t_iz if missing (e.g., from earlier version of VAST, or SpatialDeltaGLMM)
  if( !("t_iz" %in% names(TmbData)) ){
    TmbData$t_iz = matrix( TmbData$t_i, ncol=1 )
  }

  # Add in t_yz if missing (e.g., from earlier version of VAST, or SpatialDeltaGLMM)
  if( !("t_yz" %in% names(TmbData)) ){
    TmbData$t_yz = matrix(1:TmbData$n_t - 1, ncol=1)
  }

  # Extract binomial stuff for encounter-nonencounter data
  exp_rate_xy = obs_rate_xy = total_num_xy = exp_num_xy = obs_num_xy = matrix(NA, nrow=TmbData$n_x, ncol=nrow(TmbData$t_yz) )
  for( yI in 1:nrow(TmbData$t_yz) ){
    which_i_in_y = ( TmbData$t_iz == outer(rep(1,TmbData$n_i),TmbData$t_yz[yI,]) )
    which_i_in_y = which( apply(which_i_in_y,MARGIN=1,FUN=all) )
    if( length(which_i_in_y)>0 ){
      exp_rate_xy[,yI] = tapply( Report$R1_i[which_i_in_y], INDEX=factor(TmbData$s_i[which_i_in_y],levels=1:TmbData$n_x-1), FUN=mean )
      obs_rate_xy[,yI] = tapply( TmbData$b_i[which_i_in_y]>0, INDEX=factor(TmbData$s_i[which_i_in_y],levels=1:TmbData$n_x-1), FUN=mean )
      total_num_xy[,yI] = tapply( TmbData$b_i[which_i_in_y], INDEX=factor(TmbData$s_i[which_i_in_y],levels=1:TmbData$n_x-1), FUN=length )
    }else{
      total_num_xy[,yI] = 0
    }
    exp_num_xy = exp_rate_xy * total_num_xy
    obs_num_xy = obs_rate_xy * total_num_xy
  }

  # Method #1 -- Binomial cumulative function
  #Q1_xt = pbinom( obs_num_xt, size=total_num_xt, prob=exp_rate_xt )

  # Method #2 -- Pearson residuals
  Q1_xy = (obs_num_xy - exp_num_xy) / sqrt(  exp_num_xy*(total_num_xy-exp_num_xy)/total_num_xy )

  # Method #3 -- Deviance residuals
  #Q1_xt = 2*(obs_num_xt*log(obs_num_xt/exp_num_xt) + (total_num_xt-obs_num_xt)*log((total_num_xt-obs_num_xt)/(total_num_xt-exp_num_xt)))
  #Q1_xt = ifelse( obs_num_xt==0, 2*((total_num_xt-obs_num_xt)*log((total_num_xt-obs_num_xt)/(total_num_xt-exp_num_xt))), Q1_xt)
  #Q1_xt = ifelse( (total_num_xt-obs_num_xt)==0, 2*(obs_num_xt*log(obs_num_xt/exp_num_xt)), Q1_xt)

  ##################
  # Positive catch rates
  ##################

  # Extract quantile for positive catch rates
  #Q_i = Q[["Q"]]
  which_pos = which(TmbData$b_i>0)
  bvar_ipos = bpred_ipos = NULL
  # Univariate Q interface
  if( all(c("var_y","pred_y") %in% names(Q)) ){
    bvar_ipos = Q[["var_y"]]   # Change name to avoid naming-convention of y with reporting-interval
    bpred_ipos = Q[["pred_y"]]  # Change name to avoid naming-convention of y with reporting-interval
  }
  # Multivariate Q interface
  if( all(c("var_y","pred_y") %in% names(Q[[1]])) ){
    bvar_ipos = bpred_ipos = rep(NA, length=length(which_pos))
    for(i_e in 1:length(Q)){
      which_pos_and_e = which(TmbData$e_i[which_pos]==(i_e-1))
      bvar_ipos[which_pos_and_e] = Q[[i_e]][["var_y"]]
      bpred_ipos[which_pos_and_e] = Q[[i_e]][["pred_y"]]
    }
  }
  if( is.null(bvar_ipos) & is.null(bpred_ipos) ){
    stop("Something is wrong with `Q` input")
  }

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
  sum_obs_xy = sum_exp_xy = var_exp_xy = matrix(NA, nrow=TmbData$n_x, ncol=nrow(TmbData$t_yz) )
  for( yI in 1:nrow(TmbData$t_yz) ){
    which_i_in_y = ( TmbData$t_iz == outer(rep(1,TmbData$n_i),TmbData$t_yz[yI,]) )
    which_i_in_y = which( apply(which_i_in_y,MARGIN=1,FUN=all) )
    which_i_in_y_and_pos = intersect( which_i_in_y, which_pos )
    which_ipos_in_y = ( TmbData$t_iz[which_pos,] == outer(rep(1,length(which_pos)),TmbData$t_yz[yI,]) )
    which_ipos_in_y = which( apply(which_ipos_in_y,MARGIN=1,FUN=all) )
    if( length(which_i_in_y_and_pos)>0 ){
      sum_obs_xy[,yI] = tapply( TmbData$b_i[which_i_in_y_and_pos], INDEX=factor(TmbData$s_i[which_i_in_y_and_pos],levels=1:TmbData$n_x-1), FUN=sum )
      sum_exp_xy[,yI] = tapply( bpred_ipos[which_ipos_in_y], INDEX=factor(TmbData$s_i[which_i_in_y_and_pos],levels=1:TmbData$n_x-1), FUN=sum )
      var_exp_xy[,yI] = tapply( bvar_ipos[which_ipos_in_y], INDEX=factor(TmbData$s_i[which_i_in_y_and_pos],levels=1:TmbData$n_x-1), FUN=sum )
    }
  }
  Q2_xy = (sum_obs_xy - sum_exp_xy) / sqrt(var_exp_xy)

  #################
  # Plots
  #################

  if( !is.null(savedir) ){
    Col = colorRampPalette(colors=c("blue","white","red"))
    textmargin = "Pearson residual"
    for( zI in 1:2 ){
      Q_xy = list( Q1_xy, Q2_xy )[[zI]]
      zlim = c(-1,1) * ceiling(max(abs(Q_xy),na.rm=TRUE))
      #Q_xt = ifelse( abs(Q_xt)>3, 3*sign(Q_xt), Q_xt )
      SpatialDeltaGLMM:::PlotMap_Fn( MappingDetails=MappingDetails, Mat=Q_xy, PlotDF=PlotDF, Col=Col, zlim=zlim, ignore.na=TRUE, MapSizeRatio=MapSizeRatio, Xlim=Xlim, Ylim=Ylim, FileName=paste0(savedir,"/",c("maps--encounter_pearson_resid","maps--catchrate_pearson_resid")[zI]), Year_Set=paste0("Residuals--",1:ncol(Q_xy)), Rescale=Rescale, Rotate=Rotate, Format=Format, Res=Res, zone=zone, Cex=Cex, textmargin=textmargin, add=add, pch=pch, Legend=Legend, mfrow=mfrow, plot_legend_fig=plot_legend_fig, ...)
    }
  }

  #################
  # Returns
  #################

  Return = list( "Q1_xy"=Q1_xy, "Q2_xy"=Q2_xy  )  # , "obs_num_xy"=obs_num_xy, "exp_num_xy"=exp_num_xy, "total_num_xy"=total_num_xy
  return( invisible(Return) )
}
