#' Inset small plot within figure
#'
#' Inset plot with margins, background and border (based on: https://github.com/cran/berryFunctions/blob/master/R/smallPlot.R)
#'
#' @return parameters of small plot, invisible.

smallPlot <- function( expr, x=c(5,70), y=c(50,100), x1,y1,x2,y2, mar=c(12, 14, 3, 3), mgp=c(1.8, 0.8, 0),
  bg=par("bg"), border=par("fg"), las=1, resetfocus=TRUE, ...){

  # Input check:                               #  y1 | P1       |
  if(missing(x1)) x1 <- min(x, na.rm=TRUE)     #     |          |
  if(missing(x2)) x2 <- max(x, na.rm=TRUE)     #  y2 |       P2 |
  if(missing(y1)) y1 <- max(y, na.rm=TRUE)     #     ------------
  if(missing(y2)) y2 <- min(y, na.rm=TRUE)     #       x1    x2

  # catch outside plot:
  if(x1<0)  {x1 <- 0;   warning("x (",x1,") set to 0.")}
  if(y2<0)  {y2 <- 0;   warning("y (",y2,") set to 0.")}
  if(x2>100){x2 <- 100; warning("x (",x2,") set to 100.")}
  if(y1>100){y1 <- 100; warning("y (",y1,") set to 100.")}

  # control for 0:1 input:
  if(diff(range(x, na.rm=TRUE)) < 1  |  diff(range(y, na.rm=TRUE)) < 1  ){
    stop("x or y was probably given as coodinates between 0 and 1. They must be between 0 and 100.")
  }

  # old parameters to be restored at exit:
  op <- par(no.readonly=TRUE)

  # inset plot: background, border
  par(plt=c(x1, x2, y2, y1)/100, new=TRUE, mgp=mgp) # plt / fig
  plot.new() # code line from ade4::add.scatter
  u <- par("usr")
  rect(u[1], u[3], u[2], u[4], col=bg, border=border)

  # inset plot: margins
  par(plt=c(x1+mar[2], x2-mar[4], y2+mar[1], y1-mar[3])/100, new=TRUE, las=las, ...)

  # Actual plot:
  expr

  # par of small plot:
  sp <- par(no.readonly=TRUE)

  # par reset
  if(resetfocus){
    if( par("mfrow")[1]==1 & par("mfrow")[2]==1  ){
      par(op) # ruins multiple figure plots, so:
    }else{
      par(plt=op$plt, new=op$new, mgp=op$mgp, las=op$las)
    }
  }
  return(invisible(sp))
}
