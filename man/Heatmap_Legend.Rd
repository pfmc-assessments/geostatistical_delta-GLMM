\name{Heatmap_Legend}
\alias{Heatmap_Legend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Makes a legend for plotting surfaces (e.g., population density) }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Heatmap_Legend(colvec, heatrange, margintext = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colvec}{
%%     ~~Describe \code{colvec} here~~
}
  \item{heatrange}{
%%     ~~Describe \code{heatrange} here~~
}
  \item{margintext}{
%%     ~~Describe \code{margintext} here~~
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
function (colvec, heatrange, margintext = NULL) 
{
    par(xaxs = "i", yaxs = "i", mar = c(1, 0, 1, 2 + ifelse(is.null(margintext), 
        0, 1.5)), mgp = c(1.5, 0.25, 0), tck = -0.02)
    N = length(colvec)
    Y = seq(heatrange[1], heatrange[2], length = N + 1)
    plot(1, type = "n", xlim = c(0, 1), ylim = heatrange, xlab = "", 
        ylab = "", main = "", xaxt = "n", yaxt = "n", cex.main = 1.5)
    for (i in 1:N) polygon(x = c(0, 1, 1, 0), y = Y[c(i, i, i + 
        1, i + 1)], col = colvec[i], border = NA)
    axis(side = 4, at = pretty(heatrange))
    if (!is.null(margintext)) 
        mtext(side = 4, text = margintext, line = 2, cex = 1.5)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
