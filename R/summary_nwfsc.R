
#' Summarize a VAST model for Northwest Fisheries Science Center
#'
#' \code{summary_nwfsc} generates output tables requested by the PFMC SSC
#'
#' @param obj Compiled TMB object after estimating MLE of fixed effects
#' @param sdreport output from \code{TMB::sdreport}
#' @param savedir directory to save tables in CSV format

#' @return Tagged list containing objects for running a VAST model
#' \describe{
#'   \item{TableA}{Table of settings}
#'   \item{TableB}{Table of estimated parameters}
#'   \item{TableC}{Table of MLE of fixed effects}
#' }

#' @importFrom utils write.csv

#' @export
summary_nwfsc = function( obj, sdreport, savedir=NULL ){

  f = function(num,threshold=0.000001) ifelse(num<threshold,paste0("< ",threshold),num)
  # Table of settings
  TableA = data.frame( "Setting_name"=rep(NA,9), "Setting_used"=NA )
  TableA[1,] = c("Number of knots", obj$env$data$n_x)
  TableA[2,] = c("Maximum gradient", formatC(f(max(abs( obj$gr(TMB::summary.sdreport(sdreport,"fixed")[,'Estimate'])))),format="f",digits=6) )
  TableA[3,] = c("Is hessian positive definite?", switch(as.character(sdreport$pdHess),"FALSE"="No","TRUE"="Yes") )
  TableA[4,] = c("Was bias correction used?", ifelse("Est. (bias.correct)"%in%colnames(TMB::summary.sdreport(sdreport)),"Yes","No") )
  TableA[5,] = c("Distribution for measurement errors", switch(as.character(obj$env$data$ObsModel[1]),"1"="Lognormal","2"="Gamma") )
  TableA[6,] = c("Spatial effect for encounter probability", switch(as.character(obj$env$data$FieldConfig[1]),"-1"="No","1"="Yes") )
  TableA[7,] = c("Spatio-temporal effect for encounter probability", switch(as.character(obj$env$data$FieldConfig[2]),"-1"="No","1"="Yes") )
  TableA[8,] = c("Spatial effect for positive catch rate", switch(as.character(obj$env$data$FieldConfig[3]),"-1"="No","1"="Yes") )
  TableA[9,] = c("Spatio-temporal effect for positive catch rate", switch(as.character(obj$env$data$FieldConfig[4]),"-1"="No","1"="Yes") )

  # Print number of parameters
  TableB = ThorsonUtilities::list_parameters( obj, verbose=FALSE )

  # Print table of MLE of fixed effects
  TableC = TMB::summary.sdreport( sdreport, "fixed" )

  # Return
  Return = list("TableA"=TableA, "TableB"=TableB, "TableC"=TableC)
  if( !is.null(savedir)) for(i in 1:3) write.csv(Return[[i]], file=paste0(savedir,"/",names(Return)[i],".csv"), row.names=FALSE)
  return(Return)
}

