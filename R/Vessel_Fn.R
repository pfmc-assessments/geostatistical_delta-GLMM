#' @export
Vessel_Fn <-
function( TmbData, Sdreport, FileName_VYplot=NULL ){
  attach(TmbData)
  on.exit( detach(TmbData) )
  Summary = summary(Sdreport)

  nu_vt = array(NA, dim=c(n_v,n_t,2,2))
  for(vI in 1:n_v){
  for(tI in 1:n_t){
    Num = (vI-1)*n_t + tI
    nu_vt[vI,tI,1,] = Summary[which(rownames(Summary)=="nu1_vt")[Num],]
    nu_vt[vI,tI,2,] = Summary[which(rownames(Summary)=="nu2_vt")[Num],]    
  }}
  
  if( !is.null(FileName_VYplot) ) jpeg(FileName_VYplot, width=1.5*n_t,height=5,res=200,units="in")
    par(mfrow=c(2,n_t), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02, oma=c(0,3,0,0))
    for(eI in 1:2){
    for(tI in 1:n_t){
      plot(x=1:n_v, y=1:n_v, type="n", ylim=range( c(nu_vt[,,eI,1]+nu_vt[,,eI,2],nu_vt[,,eI,1]-nu_vt[,,eI,2]) ), xlab="Vessel", ylab="Effect", main=Year_Set[tI])
      if(tI==1) mtext( side=2, outer=FALSE, line=2, text=c("Presence/absence","Positive catch rate")[eI])
      for(vI in 1:n_v){
        points( x=vI, y=nu_vt[vI,tI,eI,1])
        lines( x=rep(vI,2), y=c(nu_vt[vI,tI,eI,1]-nu_vt[vI,tI,eI,2],nu_vt[vI,tI,eI,1]+nu_vt[vI,tI,eI,2])) 
      }
    }}
  if( !is.null(FileName_VYplot) ) dev.off()
  
  # Return stuff
  Return = list("nu_vt"=nu_vt)
  return( Return )
}
