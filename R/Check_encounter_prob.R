
#' @title
#' Check predicted encounter probability against observed encounter frequency
#'
#' @description
#' \code{Check_encounter_prob} is a diagnostic function for checking validity of the encounter-probability component of a spatio-temporal model
#'
#' @inheritParams PlotResultsOnMap_Fn
#' @inheritParams PlotIndex_Fn
#'
#' @return Return Tagged list of output
#' \describe{
#'   \item{Diag_i}{Diagnostic output for each sample \code{i}}
#'   \item{Diag_z}{Diagnostic output for each bin \code{z}}
#' }

#' @export
Check_encounter_prob = function( Report, Data_Geostat, cutpoints_z=seq(0,1,length=21), interval_width=1.96, DirName=paste0(getwd(),"/"),
  PlotName="Diag--Encounter_prob.png", ... ){

  # Get bin for each datum
  z_i = cut( Report$R1_i, breaks=cutpoints_z, include.lowest=TRUE )
  midpoints_z = rowMeans( cbind(cutpoints_z[-1],cutpoints_z[-length(cutpoints_z)]) )

  # Get encounter frequency for each bin
  freq_z = tapply( ifelse(Data_Geostat[,'Catch_KG']>0,1,0), INDEX=z_i, FUN=mean )

  # Get expectation given model
  num_z = tapply( Report$R1_i, INDEX=z_i, FUN=length )
  mean_z = tapply( Report$R1_i, INDEX=z_i, FUN=mean )
  var_z = tapply( Report$R1_i, INDEX=z_i, FUN=function(vec){sum(vec*(1-vec))} )
  sd_mean_z = sqrt(var_z / num_z^2)

  # Plot
  Par = list( mar=c(3,3,1,1), mgp=c(2,0.5,0), tck=-0.02, ... )
  png( file=paste0(DirName,"/",PlotName), width=5, height=5, res=200, units="in")
    par( Par )
    plot( x=midpoints_z, y=freq_z, pch=20, cex=1.2, xlim=c(0,1), ylim=c(0,1), xlab="Predicted encounter probability", ylab="Observed encounter probability" )
    SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( x=midpoints_z[which(!is.na(mean_z))], y=mean_z[which(!is.na(mean_z))], ybounds=(mean_z%o%c(1,1)+sd_mean_z%o%c(-interval_width,interval_width))[which(!is.na(mean_z)),], lwd=2, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red" )
    abline(a=0, b=1, lty="dotted", lwd=2 )
    legend( "topleft", legend=c("Observed","Predicted"), fill=c("black","red"), bty="n")
  dev.off()

  # Return stuff
  Return = NULL
  Return[["Diag_i"]] = cbind("b_i"=Data_Geostat[,'Catch_KG'], "z_i"=z_i)
  Return[["Diag_z"]] = cbind("midpoints_z"=midpoints_z, "freq_z"=freq_z, "num_z"=num_z, "mean_z"=mean_z, "var_z"=var_z, "sd_mean_z"=sd_mean_z)
  return( invisible(Return) )
}

