Heatmap_Legend <-
function( colvec, heatrange, textmargin=NULL, labeltransform="uniform", dopar=TRUE, side=4 ){
  if(dopar==TRUE) par( xaxs="i", yaxs="i", mar=c(1,0,1,2+ifelse(is.null(textmargin),0,1.5)), mgp=c(1.5,0.25,0), tck=-0.02 )
  N = length(colvec)
  Y = seq(heatrange[1], heatrange[2], length=N+1)
  plot( 1, type="n", xlim=c(0,1), ylim=heatrange, xlab="", ylab="", main="", xaxt="n", yaxt="n", cex.main=1.5, xaxs="i", yaxs="i")
  for( i in 1:N) polygon( x=c(0,1,1,0), y=Y[c(i,i,i+1,i+1)], col=colvec[i], border=NA)
  if( labeltransform=="uniform" ) Labels = pretty(heatrange)
  if( labeltransform=="inv_log10" ) Labels = 10^pretty(heatrange)
  axis(side=4, at=pretty(heatrange), labels=Labels )
  if(!is.null(textmargin)) mtext(side=side, text=textmargin, line=2, cex=1.5, las=0)
  #mtext(side=1, outer=TRUE, line=1, "Legend")
}
