
#' @export
Plot_range_shifts = function( Sdreport, Report, TmbData, Year_Set=NULL, FileName_COG=paste0(getwd(),"/center_of_gravity.png"), FileName_Area=paste0(getwd(),"/Area.png"), Znames=rep("",ncol(Report$mean_Z_tm)), use_biascorr=TRUE, ...){
  # Default inputs
  if( is.null(Year_Set)) Year_Set = 1:TmbData$n_t

  # Only run if necessary outputs are available
  if( "mean_Z_tm" %in% names(Report) ){
    # Extract estimates
    SD = summary(Sdreport)
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
    png( file=FileName_COG, width=6.5, height=3, res=200, units="in")
      par( mfrow=c(1,ncol(mean_Z_tm)), mar=c(3,2,2,0), mgp=c(1.75,0.25,0), tck=-0.02)  # , ...
      for(m in 1:ncol(mean_Z_tm)){
        WhichRows = 1:TmbData$n_t + TmbData$n_t*(m-1)
        Ybounds = (SD_mean_Z_tm[WhichRows,'Estimate']%o%c(1,1) + SD_mean_Z_tm[WhichRows,'Std. Error']%o%c(-1,1))
        Ylim = range(Ybounds,na.rm=TRUE)
        SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn(x=Year_Set, y=SD_mean_Z_tm[WhichRows,1], ybounds=Ybounds, col_bounds=rgb(1,0,0,0.2), fn=plot, type="l", lwd=2, col="red", bounds_type="shading", ylim=Ylim, xlab="Year", ylab=ifelse(m==1,"Location",""), main=Znames[m])
      }
    dev.off()

    # Plot area
    png( file=FileName_Area, width=4, height=4, res=200, units="in")
      par( mfrow=c(1,1), mar=c(3,3,2,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(0,0,0,0))
      SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn( x=Year_Set, y=SD_log_area_Z_tmm[,2,1,1], ybounds=SD_log_area_Z_tmm[,2,1,1]%o%rep(1,2)+SD_log_area_Z_tmm[,2,1,2]%o%c(-1,1), fn=plot, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red", lwd=2, xlab="Year", ylab="log(km^2)", type="l", main="Total area")
    dev.off()

    # Return stuff
    Return = list("mean_Z_tm"=mean_Z_tm, "SD_mean_Z_tm"=SD_mean_Z_tm, "Year_Set"=Year_Set)
    return( invisible(Return) )
  }else{
    message( "To estimate range-shifts, please re-run with Options['Calculate_Range']=1" )
  }
}

