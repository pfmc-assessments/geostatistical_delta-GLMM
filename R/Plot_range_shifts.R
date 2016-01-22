
Plot_range_shifts = function( Sdreport, Report, FileName_COG=paste0(getwd(),"/center_of_gravity.png"), Znames=rep("",ncol(Report$mean_Z_tm)), use_biascorr=TRUE, ...){
  if( "mean_Z_tm" %in% names(Report) ){
    SD = summary(Sdreport)
    mean_Z_tm = Report$mean_Z_tm
    SD_mean_Z_tm = SD[which(rownames(SD)=="mean_Z_tm"),]
    SD_mean_relative_Z_tm = SD[which(rownames(SD)=="mean_relative_Z_tm"),]
    # bias correction
    if( use_biascorr==TRUE & ("unbiased"%in%names(Sdreport)) ){
      message("Using bias-corrected estimates for center of gravity")
      SD_mean_Z_tm[,'Estimate'] = Sdreport$unbiased$value[ grep("mean_Z_tm",names(Sdreport$unbiased$value)) ]
      SD_mean_relative_Z_tm[,'Estimate'] = Sdreport$unbiased$value[ grep("mean_relative_Z_tm",names(Sdreport$unbiased$value)) ]
    }
    # Plot
    png( file=FileName_COG, width=6.5, height=3, res=200, units="in")
      par( mfrow=c(1,ncol(mean_Z_tm)), mar=c(3,2,2,0), mgp=c(1.75,0.25,0), tck=-0.02)  # , ...
      for(m in 1:ncol(mean_Z_tm)){
        WhichRows = 1:TmbData$n_t + TmbData$n_t*(m-1)
        Ybounds = (SD_mean_Z_tm[WhichRows,'Estimate']%o%c(1,1) + SD_mean_Z_tm[WhichRows,'Std. Error']%o%c(-1,1))
        Ylim = range(Ybounds,na.rm=TRUE)
        Plot_Points_and_Bounds_Fn(x=Year_Set, y=SD_mean_Z_tm[WhichRows,1], ybounds=Ybounds, col_bounds=rgb(1,0,0,0.2), fn=plot, type="l", lwd=2, col="red", bounds_type="shading", ylim=Ylim, xlab="Year", ylab=ifelse(m==1,"Location",""), main=Znames[m])
      }
    dev.off()
    # Return stuff
    Return = list("mean_Z_tm"=mean_Z_tm, "SD_mean_Z_tm"=SD_mean_Z_tm)
    return( invisible(Return) )
  }else{
    message( "To estimate range-shifts, please re-run with Options['Calculate_Range']=1" )
  }
}

