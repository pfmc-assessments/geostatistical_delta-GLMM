\name{PlotAniso_Fn}
\alias{PlotAniso_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots anisotropy kernal for Gaussian random fields }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotAniso_Fn(FileName, Report, ControlList = list(Width = 5, Height = 5, Res = 200, Units = "in"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
%%     ~~Describe \code{FileName} here~~
}
  \item{Report}{
%%     ~~Describe \code{Report} here~~
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
function (FileName, Report, ControlList = list(Width = 5, Height = 5, 
    Res = 200, Units = "in")) 
{
    png(file = FileName, width = ControlList$Width, height = ControlList$Height, 
        res = ControlList$Res, units = ControlList$Units)
    par(mar = c(2, 2, 0, 0), mgp = c(1.5, 0.5, 0), tck = -0.02)
    Eigen = eigen(Report$H)
    plot(1, type = "n", xlim = c(-1, 1) * max(Eigen$values), 
        ylim = c(-1, 1) * max(Eigen$values))
    arrows(x0 = rep(0, 2), y0 = rep(0, 2), x1 = Eigen$vectors[1, 
        ] * Eigen$values, y1 = Eigen$vectors[2, ] * Eigen$values)
    dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
