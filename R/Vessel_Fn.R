
#' @title
#' Plot vessel effects
#'
#' @description
#' \code{Vessel_Fn} plots estimated vessel effects for model components
#'
#' @param TmbData Formatted data inputs, from `SpatialDeltaGLMM::Data_Fn(...)`
#' @param Sdreport TMB output from `TMB::sdreport(Obj)`
#' @param FileName_VYplot Full filename (including directory) for center-of-gravity plot
#'
#' @return Return Tagged list of output
#'

#' @export
Vessel_Fn <-
function( TmbData, Sdreport, FileName_VYplot=NULL ){
  Summary = TMB:::summary.sdreport(Sdreport)
  Return = NULL

  if( !any(c("nu1_vt","nu2_vt") %in% rownames(Summary)) ){
    message( "Not plotting vessel effects because none are present" )
  }else{
    message( "\nPlotting vessel effects..." )
    nu_vt = array(NA, dim=c(TmbData$n_v, TmbData$n_t, 2, 2))
    for(vI in 1:TmbData$n_v){
    for(tI in 1:TmbData$n_t){
      Num = (vI-1)*TmbData$n_t + tI
      nu_vt[vI,tI,1,] = Summary[which(rownames(Summary)=="nu1_vt")[Num],]
      nu_vt[vI,tI,2,] = Summary[which(rownames(Summary)=="nu2_vt")[Num],]
    }}

    # Make plot
    if( !is.null(FileName_VYplot) ) jpeg(FileName_VYplot, width=1.5*TmbData$n_t,height=5,res=200,units="in")
      par(mfrow=c(2,TmbData$n_t), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02, oma=c(0,3,0,0))
      for(eI in 1:2){
      for(tI in 1:TmbData$n_t){
        plot(x=1:TmbData$n_v, y=1:TmbData$n_v, type="n", ylim=range( c(nu_vt[,,eI,1]+nu_vt[,,eI,2],nu_vt[,,eI,1]-nu_vt[,,eI,2]) ), xlab="Vessel", ylab="Effect", main=TmbData$Year_Set[tI])
        if(tI==1) mtext( side=2, outer=FALSE, line=2, text=c("Presence/absence","Positive catch rate")[eI])
        for(vI in 1:TmbData$n_v){
          points( x=vI, y=nu_vt[vI,tI,eI,1])
          lines( x=rep(vI,2), y=c(nu_vt[vI,tI,eI,1]-nu_vt[vI,tI,eI,2],nu_vt[vI,tI,eI,1]+nu_vt[vI,tI,eI,2]))
        }
      }}
    if( !is.null(FileName_VYplot) ) dev.off()

    # Add stuff
    Return = c(Return, list("nu_vt"=nu_vt) )
  }

  # Return stuff
  return( Return )
}
