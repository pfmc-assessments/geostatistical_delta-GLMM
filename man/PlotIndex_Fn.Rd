\name{PlotIndex_Fn}
\alias{PlotIndex_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make plot of geostatistical index by strata
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotIndex_Fn(PlotName = "Index.png", DirName, TmbData, Sdreport, Year_Set, strata.limits, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PlotName}{
%%     ~~Describe \code{PlotName} here~~
}
  \item{DirName}{
%%     ~~Describe \code{DirName} here~~
}
  \item{TmbData}{
%%     ~~Describe \code{TmbData} here~~
}
  \item{Sdreport}{
%%     ~~Describe \code{Sdreport} here~~
}
  \item{Year_Set}{
%%     ~~Describe \code{Year_Set} here~~
}
  \item{strata.limits}{
%%     ~~Describe \code{strata.limits} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (PlotName = "Index.png", DirName, TmbData, Sdreport, 
    Year_Set, strata.limits, ...) 
{
    png(file = paste0(DirName, "/", PlotName), width = 4, height = 4, 
        res = 200, units = "in")
    par(mar = c(3, 3, 2, 0), mgp = c(2, 0.5, 0), tck = -0.02, 
        yaxs = "i", ...)
    log_Index = array(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
        "ln_Index_tl"), ], dim = c(unlist(TmbData[c("n_t", "n_l")]), 
        2), dimnames = list(NULL, NULL, c("Estimate", "Std. Error")))
    Index = array(summary(Sdreport)[which(rownames(summary(Sdreport)) == 
        "Index_tl"), ], dim = c(unlist(TmbData[c("n_t", "n_l")]), 
        2), dimnames = list(NULL, NULL, c("Estimate", "Std. Error")))
    plot(1, type = "n", xlim = range(Year_Set), ylim = 1.05 * 
        c(0, max(exp(log_Index[, , "Estimate"] + 1 * log_Index[, 
            , "Std. Error"]))), xlab = "Year", ylab = "Abundance")
    for (l in 1:dim(Index)[2]) {
        lines(y = Index[, l, "Estimate"], x = Year_Set + seq(-0.1, 
            0.1, length = dim(Index)[2])[l], type = "b", col = rainbow(TmbData[["n_l"]])[l])
        for (t in 1:dim(Index)[1]) {
            lines(x = rep(Year_Set[t], 2) + seq(-0.1, 0.1, length = dim(Index)[2])[l], 
                y = exp(log_Index[t, l, "Estimate"] + c(-1, 1) * 
                  log_Index[t, l, "Std. Error"]), col = rainbow(TmbData[["n_l"]])[l])
        }
    }
    Names = strata.limits[, "STRATA"]
    Table = data.frame(Year = Year_Set, Unit = 1, Fleet = rep(Names, 
        each = dim(Index)[1]), Estimate = as.vector(Index[, , 
        "Estimate"]), SD = as.vector(log_Index[, , "Std. Error"]))
    write.csv(Table, file = paste0(DirName, "/Table_for_SS3.csv"), 
        row.names = FALSE)
    dev.off()
    Return = Table
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
