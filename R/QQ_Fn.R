#' @export
QQ_Fn <-
function(TmbData, Report, FileName_PP=NULL, FileName_Phist=NULL, FileName_QQ=NULL, FileName_Qhist=NULL){
  pow = function(a,b) a^b
  Which = which(TmbData$b_i>0)
  Q = rep(NA, length(Which) ) # vector to track quantiles for each observation
  y = array(NA, dim=c(length(Which),1000))
  pred_y = var_y = rep(NA, length(Which) ) # vector to track quantiles for each observation

  # Make plot while calculating posterior predictives
  if( !is.null(FileName_PP) ) jpeg(FileName_PP, width=10, height=3, res=200, units="in")
    par(mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    plot( TmbData$b_i[Which], ylab="", xlab="",log="y", main="", col="blue")
    # Loop through observations
    for(ObsI in 1:length(Which)){
      # Calculate pred_y
        # I can't use R2_i anymore because interpretation changed around March 9, 2017 (due to area-swept change in Poisson-process and Tweedie functions)
        # However, I CAN use P2_i, which has a stable definition over time (as a linear predictor)
      if( length(TmbData$ObsModel)==1 || TmbData$ObsModel[2]==0 ){
        pred_y[ObsI] = TmbData$a_i[Which[ObsI]] * exp(Report$P2_i[Which[ObsI]])
      }
      if( length(TmbData$ObsModel)>=2 || TmbData$ObsModel[2]==1 ){
        R1_i = 1 - exp( -1 * Report$SigmaM[TmbData$c_i[Which[ObsI]],3] * exp(Report$P1_i[Which[ObsI]]) )
        pred_y[ObsI] = exp(Report$P1_i[Which[ObsI]]) / R1_i * exp(Report$P2_i[Which[ObsI]]);
      }
      if( length(TmbData$ObsModel)>=2 || TmbData$ObsModel[2]==2 ){
        pred_y[ObsI] = TmbData$a_i[Which[ObsI]] * exp(Report$P2_i[Which[ObsI]])
      }
      if( !(TmbData$ObsModel[1] %in% c(1,2,11,12)) ) stop("QQ not working except for when TmbData$ObsModel[1] is 1, 2, 11, or 12")
      # Simulate quantiles for different distributions
      if(TmbData$ObsModel[1]==1){
        y[ObsI,] = rlnorm(n=ncol(y), meanlog=log(pred_y[ObsI])-pow(Report$SigmaM[1],2)/2, sdlog=Report$SigmaM[1])   # Plotting in log-space
        Q[ObsI] = plnorm(q=TmbData$b_i[Which[ObsI]], meanlog=log(pred_y[ObsI])-pow(Report$SigmaM[1],2)/2, sdlog=Report$SigmaM[1])
      }
      if(TmbData$ObsModel[1]==2){
        b = pow(Report$SigmaM[1],2) * pred_y[ObsI];
        y[ObsI,] = rgamma(n=ncol(y), shape=1/pow(Report$SigmaM[1],2), scale=b)
        Q[ObsI] = pgamma(q=TmbData$b_i[Which[ObsI]], shape=1/pow(Report$SigmaM[1],2), scale=b)
      }
      if(TmbData$ObsModel[1]==11){
        ECE = rbinom(n=1000, size=1, prob=1-Report$SigmaM[2])
        y[ObsI,] = rlnorm(n=ncol(y), meanlog=log(pred_y[ObsI])-pow(Report$SigmaM[1],2)/2, sdlog=Report$SigmaM[1])*(1-ECE) + rlnorm(n=ncol(y), meanlog=log(pred_y[ObsI])-pow(Report$SigmaM[4],2)/2+log(1+Report$SigmaM[3]), sdlog=Report$SigmaM[4])*ECE
        Q[ObsI] = plnorm(q=TmbData$b_i[Which[ObsI]], meanlog=log(pred_y[ObsI])-pow(Report$SigmaM[1],2)/2, sdlog=Report$SigmaM[1])*Report$SigmaM[2] + plnorm(q=TmbData$b_i[Which[ObsI]], meanlog=log(pred_y[ObsI])-pow(Report$SigmaM[4],2)/2+log(1+Report$SigmaM[3]), sdlog=Report$SigmaM[4])*(1-Report$SigmaM[2])
      }
      if(TmbData$ObsModel[1]==12){
        b = pow(Report$SigmaM[1],2) * pred_y[ObsI];
        b2 = pow(Report$SigmaM[4],2) * pred_y[ObsI] * (1+Report$SigmaM[3]);
        ECE = rbinom(n=ncol(y), size=1, prob=1-Report$SigmaM[2])
        y[ObsI,] = rgamma(n=ncol(y), shape=1/pow(Report$SigmaM[1],2), scale=b)*(1-ECE) + rgamma(n=ncol(y), shape=1/pow(Report$SigmaM[4],2), scale=b2)*ECE
        Q[ObsI] = pgamma(q=TmbData$b_i[Which[ObsI]], shape=1/pow(Report$SigmaM[1],2), scale=b)*Report$SigmaM[2] + pgamma(q=TmbData$b_i[Which[ObsI]], shape=1/pow(Report$SigmaM[4],2), scale=b2)*(1-Report$SigmaM[2])
      }
      # Add results to plot
      var_y[ObsI] = var( y[ObsI,] )
      Quantiles = quantile(y[ObsI,],prob=c(0.025,0.25,0.75,0.975))
      lines(x=c(ObsI,ObsI), y=Quantiles[2:3], lwd=2)
      lines(x=c(ObsI,ObsI), y=Quantiles[c(1,4)], lwd=1,lty="dotted")
      if(TmbData$b_i[Which[ObsI]]>max(Quantiles) | TmbData$b_i[Which[ObsI]]<min(Quantiles)){
        points(x=ObsI,y=TmbData$b_i[Which[ObsI]],pch=4,col="red",cex=2)
      }
    }
  if( !is.null(FileName_PP) ) dev.off()
  
  # Q-Q plot
  if( !is.null(FileName_QQ) ) jpeg(FileName_QQ, width=4, height=4, res=200, units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    Qtemp = na.omit(Q)
    Order = order(Qtemp)
    plot(x=seq(0,1,length=length(Order)), y=Qtemp[Order], main="Q-Q plot", xlab="Uniform", ylab="Empirical", type="l", lwd=3)
    abline(a=0,b=1)
  if( !is.null(FileName_QQ) ) dev.off()
  
  # Aggregate predictive distribution
  if( !is.null(FileName_Phist) ) jpeg(FileName_Phist, width=4, height=4, res=200, units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    hist( log(y), main="Aggregate predictive dist.", xlab="log(Obs)", ylab="Density")
  if( !is.null(FileName_Phist) ) dev.off()
  
  # Quantile histogram
  if( !is.null(FileName_Qhist) ) jpeg(FileName_Qhist, width=4, height=4, res=200, units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    hist(na.omit(Q), main="Quantile_histogram", xlab="Quantile", ylab="Number")
  if( !is.null(FileName_Qhist) ) dev.off()
  
  # Return stuff
  Return = list( "Q"=Q, "var_y"=var_y, "pred_y"=pred_y )
  return( Return )
}
