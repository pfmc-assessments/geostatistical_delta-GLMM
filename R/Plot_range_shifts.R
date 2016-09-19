
#' @title
#' Plot shifts in distribution and area occupied
#'
#' @description
#' \code{Plot_range_shifts} plots center-of-gravity, kernel-area occupied, and effective-area occupied
#'
#' @param Sdreport TMB output from `TMB::sdreport(Obj)`
#' @param Report Reporting output from `Obj$report()`
#' @param TmbData Formatted data inputs, from `SpatialDeltaGLMM::Data_Fn(...)`
#' @param Year_Set Year names for plotting
#' @param PlotDir Directory for plots
#' @param FileName_COG Full filename (including directory) for center-of-gravity plot
#' @param FileName_Area Full filename (including directory) for center-of-gravity plot
#' @param FileName_EffArea Full filename (including directory) for center-of-gravity plot
#' @param Znames Names for center-of-gravity summary statistics
#' @inheritParams PlotIndex_Fn
#'
#' @return Return Tagged list of output
#'

#' @export
Plot_range_shifts = function( Sdreport, Report, TmbData, Year_Set=NULL, PlotDir=paste0(getwd(),"/"), FileName_COG=paste0(PlotDir,"/center_of_gravity.png"), FileName_Area=paste0(PlotDir,"/Area.png"), FileName_EffArea=paste0(PlotDir,"/Effective_Area.png"), Znames=rep("",ncol(Report$mean_Z_tm)), use_biascorr=TRUE, ...){
  # Default inputs
  if( is.null(Year_Set)) Year_Set = 1:TmbData$n_t
  Return = list( "Year_Set"=Year_Set )

  # Plot distribution shift and kernal-area approximation to area occupied if necessary outputs are available
  if( !("mean_Z_tm" %in% names(Report)) ){
    message( "To plot range-shifts and kernal-approximation to area occupied, please re-run with Options['Calculate_Range']=1" )
  }else{
    message( "\nPlotting range-shifts and kernal-approximation to area occupied..." )
    # Extract estimates
    SD = TMB:::summary.sdreport(Sdreport)
    mean_Z_tm = Report$mean_Z_tm
    SD_mean_Z_tm = SD[which(rownames(SD)=="mean_Z_tm"),]
    SD_mean_relative_Z_tm = SD[which(rownames(SD)=="mean_relative_Z_tm"),]
    SD_log_area_Z_tmm = array(SD[which(rownames(SD)=="log_area_Z_tmm"),], dim=c(dim(Report$area_Z_tmm),2))

    # bias correction
    if( use_biascorr==TRUE & ("unbiased"%in%names(Sdreport)) ){
      message("Using bias-corrected estimates for center of gravity")
      SD_mean_Z_tm[,'Estimate'] = Sdreport$unbiased$value[ grep("mean_Z_tm",names(Sdreport$unbiased$value)) ]
      SD_mean_relative_Z_tm[,'Estimate'] = Sdreport$unbiased$value[ grep("mean_relative_Z_tm",names(Sdreport$unbiased$value)) ]
    }

    # Plot center of gravity
    Table = NULL
    png( file=FileName_COG, width=6.5, height=3, res=200, units="in")
      par( mfrow=c(1,ncol(mean_Z_tm)), mar=c(3,2,2,0), mgp=c(1.75,0.25,0), tck=-0.02)  # , ...
      for(m in 1:ncol(mean_Z_tm)){
        WhichRows = 1:TmbData$n_t + TmbData$n_t*(m-1)
        Ybounds = (SD_mean_Z_tm[WhichRows,'Estimate']%o%c(1,1) + SD_mean_Z_tm[WhichRows,'Std. Error']%o%c(-1,1))
        Ylim = range(Ybounds,na.rm=TRUE)
        SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn(x=Year_Set, y=SD_mean_Z_tm[WhichRows,1], ybounds=Ybounds, col_bounds=rgb(1,0,0,0.2), fn=plot, type="l", lwd=2, col="red", bounds_type="shading", ylim=Ylim, xlab="Year", ylab=ifelse(m==1,"Location",""), main=Znames[m])
        Table = rbind(Table, cbind("m"=m, "Year"=Year_Set, "COG_hat"=SD_mean_Z_tm[WhichRows,'Estimate'], "SE"=SD_mean_Z_tm[WhichRows,'Std. Error']))
      }
    dev.off()

    # Plot area
    png( file=FileName_Area, width=4, height=4, res=200, units="in")
      par( mfrow=c(1,1), mar=c(3,3,2,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(0,0,0,0))
      SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( x=Year_Set, y=SD_log_area_Z_tmm[,2,1,1], ybounds=SD_log_area_Z_tmm[,2,1,1]%o%rep(1,2)+SD_log_area_Z_tmm[,2,1,2]%o%c(-1,1), fn=plot, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red", lwd=2, xlab="Year", ylab="ln(km^2)", type="l", main="Kernel approximation to area occupied")
    dev.off()

    # Return stuff
    Return = c(Return, list("mean_Z_tm"=mean_Z_tm, "SD_mean_Z_tm"=SD_mean_Z_tm, "Table"=Table))
  }

  #
  # Only run if necessary outputs are available
  if( !("effective_area_tl" %in% names(Report)) ){
    message( "To plot effective area occupied, please re-run with Options['Calculate_effective_area']=1" )
  }else{
    message( "\nPlotting effective area occupied..." )
    # Extract estimates
    SD = TMB:::summary.sdreport(Sdreport)
    effective_area_tl = Report$mean_Z_tm
    SD_effective_area_tl = array(SD[which(rownames(SD)=="effective_area_tl"),], dim=c(dim(Report$effective_area_tl),2))
    SD_log_effective_area_tl = array(SD[which(rownames(SD)=="log_effective_area_tl"),], dim=c(dim(Report$effective_area_tl),2))

    # bias correction
    if( use_biascorr==TRUE & ("unbiased"%in%names(Sdreport)) ){
      message("Using bias-corrected estimates for effective area occupied")
      SD_effective_area_tl[,'Estimate'] = Sdreport$unbiased$value[ grep("SD_effective_area_tl",names(Sdreport$unbiased$value)) ]
    }

    # Plot area
    png( file=FileName_EffArea, width=4, height=4, res=200, units="in")
      par( mfrow=c(1,1), mar=c(3,3,2,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(0,0,0,0))
      SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( x=Year_Set, y=SD_log_effective_area_tl[,1,1], ybounds=SD_log_effective_area_tl[,1,1]%o%rep(1,2)+SD_log_effective_area_tl[,1,2]%o%c(-1,1), fn=plot, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red", lwd=2, xlab="Year", ylab="ln(km^2)", type="l", main="Effective area occupied")
    dev.off()

    # Return stuff
    Return = c(Return, list("effective_area_tl"=effective_area_tl, "SD_effective_area_tl"=SD_effective_area_tl, "SD_log_effective_area_tl"=SD_log_effective_area_tl))
  }

  # Return list of stuff
  return( invisible(Return) )
}

