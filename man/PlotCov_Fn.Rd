\name{PlotCov_Fn}
\alias{PlotCov_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots impact of covariate on surfaces }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotCov_Fn(Report, NN_Extrap, X_xj, FileName, ControlList = list(Width = 5 * 3, Height = 2 * 3, Res = 200, Units = "in"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Report}{
%%     ~~Describe \code{Report} here~~
}
  \item{NN_Extrap}{
%%     ~~Describe \code{NN_Extrap} here~~
}
  \item{X_xj}{
%%     ~~Describe \code{X_xj} here~~
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
function (Report, NN_Extrap, X_xj, FileName, ControlList = list(Width = 5 * 
    3, Height = 2 * 3, Res = 200, Units = "in")) 
{
    D_it = Report$D_xt[NN_Extrap$nn.idx, ]
    R1_it = Report$R1_xt[NN_Extrap$nn.idx, ]
    R2_it = Report$R2_xt[NN_Extrap$nn.idx, ]
    for (CovI in 1:ncol(X_xj)) {
        png(file = paste0(FileName, colnames(X_xj)[CovI], ".png"), 
            width = ControlList$Width, height = ControlList$Height, 
            res = ControlList$Res, units = ControlList$Units)
        par(mfrow = c(2, 6), oma = c(0, 0, 2, 0), mar = c(2, 
            2, 0, 0), mgp = c(1.5, 0.5, 0), tck = -0.02)
        for (t in 1:length(Year_Set)) {
            plot(x = X_xj[NN_Extrap$nn.idx, CovI], y = R2_it[, 
                t], type = "p", col = "red", ylim = c(0, max(R2_it)))
            points(x = X_xj[NN_Extrap$nn.idx, CovI], y = R1_it[, 
                t] * max(R2_it), type = "p", col = "blue")
            points(x = X_xj[NN_Extrap$nn.idx, CovI], y = D_it[, 
                t], type = "p", col = "black")
        }
        mtext(side = 3, outer = TRUE, colnames(X_xj)[CovI])
        dev.off()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
