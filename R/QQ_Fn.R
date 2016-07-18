#' @export
QQ_Fn <-
function(TmbData, Report, FileName_PP=NULL, FileName_Phist=NULL, FileName_QQ=NULL, FileName_Qhist=NULL){
  pow = function(a,b) a^b
  Which = which(TmbData$b_i>0)
  Q = rep(NA, length(Which) ) # vector to track quantiles for each observation
  y = array(NA, dim=c(length(Which),1000))
  
  # Make plot while calculating posterior predictives
  if( !is.null(FileName_PP) ) jpeg(FileName_PP,width=10,height=3,res=200,units="in")
    par(mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    plot( TmbData$b_i[Which], ylab="", xlab="",log="y", main="", col="blue")
    # mean(u.nz[,2])
    for(ObsI in 1:length(Which)){
      Pred = (Report$R2_i[Which[ObsI]]*TmbData$a_i[Which[ObsI]])
      if(TmbData$ObsModel==1){     
        y[ObsI,] = rlnorm(n=ncol(y), meanlog=log(Pred)-pow(Report$SigmaM[1],2)/2, sdlog=Report$SigmaM[1])   # Plotting in log-space
      }
      if(TmbData$ObsModel==2){     
        b = pow(Report$SigmaM[1],2) * Pred;
        y[ObsI,] = rgamma(n=ncol(y), shape=1/pow(Report$SigmaM[1],2), scale=b)
      }
      if(TmbData$ObsModel==11){     
        ECE = rbinom(n=1000, size=1, prob=1-Report$SigmaM[2])
        y[ObsI,] = rlnorm(n=ncol(y), meanlog=log(Pred)-pow(Report$SigmaM[1],2)/2, sdlog=Report$SigmaM[1])*(1-Report$ECE) + rlnorm(n=ncol(y), meanlog=log(Pred)-pow(Report$SigmaM[4],2)/2+log(1+Report$SigmaM[3]), sdlog=Report$SigmaM[4])*Report$ECE
      }
      if(TmbData$ObsModel==12){     
        b = pow(Report$SigmaM[1],2) * Pred;
        b2 = pow(Report$SigmaM[4],2) * Pred * (1+Report$SigmaM[3]);
        ECE = rbinom(n=ncol(y), size=1, prob=1-Report$SigmaM[2])
        y[ObsI,] = rgamma(n=ncol(y), shape=1/pow(Report$SigmaM[1],2), scale=b)*(1-Report$ECE) + rgamma(n=ncol(y), shape=1/pow(Report$SigmaM[4],2), scale=b2)*Report$ECE
      }
      Q[ObsI] = mean(y[ObsI,] > TmbData$b_i[Which[ObsI]])
      Quantiles = quantile(y[ObsI,],prob=c(0.025,0.25,0.75,0.975))
      lines(x=c(ObsI,ObsI), y=Quantiles[2:3], lwd=2)
      lines(x=c(ObsI,ObsI), y=Quantiles[c(1,4)], lwd=1,lty="dotted")
      if(TmbData$b_i[Which[ObsI]]>max(Quantiles) | TmbData$b_i[Which[ObsI]]<min(Quantiles)){
        points(x=ObsI,y=TmbData$b_i[Which[ObsI]],pch=4,col="red",cex=2)
      }
    }
  if( !is.null(FileName_PP) ) dev.off()
  
  # Q-Q plot
  if( !is.null(FileName_QQ) ) jpeg(FileName_QQ,width=4,height=4,res=200,units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    Qtemp = na.omit(Q)
    Order = order(Qtemp)
    plot(x=seq(0,1,length=length(Order)), y=Qtemp[Order], main="Q-Q plot", xlab="Uniform", ylab="Empirical")
    abline(a=0,b=1)
  if( !is.null(FileName_QQ) ) dev.off()
  
  # Aggregate predictive distribution
  if( !is.null(FileName_Phist) ) jpeg(FileName_Phist,width=4,height=4,res=200,units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    hist( log(y), main="Aggregate predictive dist.", xlab="log(Obs)", ylab="Density")
  if( !is.null(FileName_Phist) ) dev.off()
  
  # Quantile histogram
  if( !is.null(FileName_Qhist) ) jpeg(FileName_Qhist,width=4,height=4,res=200,units="in")
    par(mfrow=c(1,1), mar=c(2,2,2,0), mgp=c(1.25,0.25,0), tck=-0.02)
    hist(na.omit(Q), main="Quantile_histogram", xlab="Quantile", ylab="Number")
  if( !is.null(FileName_Qhist) ) dev.off()
  
  # Return stuff
  Return = list( "Q"=Q )
  return( Return )
}
