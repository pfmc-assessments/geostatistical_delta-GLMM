\name{Timeseries_Fn}
\alias{Timeseries_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generates a summary of model fit by plotting estimated time series}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Timeseries_Fn(Report, FileName, ControlList = list(Width = 4 * 3, Height = 2 * 3, Res = 200, Units = "in"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Report}{
%%     ~~Describe \code{Report} here~~
}
  \item{FileName}{
%%     ~~Describe \code{FileName} here~~
}
  \item{ControlList}{
%%     ~~Describe \code{ControlList} here~~
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
function (Report, FileName, ControlList = list(Width = 4 * 3, 
    Height = 2 * 3, Res = 200, Units = "in")) 
{
    png(file = FileName, width = ControlList$Width, height = ControlList$Height, 
        res = ControlList$Res, units = ControlList$Units)
    par(mfrow = c(2, 4), oma = c(0, 0, 0, 0), mar = c(3, 3, 2, 
        0), mgp = c(1.5, 0.5, 0), tck = -0.02)
    EntropyFn = function(Vec) {
        sum(Vec * log(Vec + 1e-250)/log(length(Vec)))
    }
    Entropy_t = apply(Report$D_xt, MARGIN = 2, FUN = EntropyFn)
    plot(x = Year_Set, y = Entropy_t, type = "l", main = "Entropy")
    Var_t = apply(Report$D_xt, MARGIN = 2, FUN = var)
    plot(x = Year_Set, y = Var_t, type = "l", main = "Variance", 
        ylim = c(0, max(Var_t)))
    CV_t = apply(Report$D_xt, MARGIN = 2, FUN = function(Vec) {
        sd(Vec)/mean(Vec)
    })
    plot(x = Year_Set, y = CV_t, type = "l", main = "CV", ylim = c(0, 
        max(CV_t)))
    Occup_t = colMeans(R1_it)
    plot(x = Year_Set, y = Occup_t, type = "l", main = "Occup_t", 
        ylim = c(0, 1))
    CondDens_t = colMeans(R2_it)
    plot(x = Year_Set, y = CondDens_t, type = "l", main = "CondDens_t", 
        ylim = c(0, max(CondDens_t)))
    Index_t = colMeans(D_it)
    plot(x = Year_Set, y = Index_t, type = "l", main = "Index_t", 
        ylim = c(0, max(Index_t)))
    Cor_t = sapply(1:length(Year_Set), FUN = function(Num) {
        cor(R1_it[, Num], R2_it[, Num], method = "spearman")
    })
    plot(x = Year_Set, y = Cor_t, type = "l", main = "Cor_t", 
        ylim = c(-1, 1))
    abline(h = 0, lty = "dotted")
    dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
