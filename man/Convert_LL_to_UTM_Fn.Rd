\name{Convert_LL_to_UTM_Fn}
\alias{Convert_LL_to_UTM_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Latitute/Longitude to UTM (i.e. Eastings-Northings)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Convert_LL_to_UTM_Fn(Lon, Lat, zone = "10")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Lon}{
%%     ~~Describe \code{Lon} here~~
}
  \item{Lat}{
%%     ~~Describe \code{Lat} here~~
}
  \item{zone}{
%%     ~~Describe \code{zone} here~~
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
function (Lon, Lat, zone = "10") 
{
    Tmp = cbind(PID = 1, POS = 1:length(Lon), X = Lon, Y = Lat)
    attr(Tmp, "projection") = "LL"
    attr(Tmp, "zone") = zone
    tmpUTM = convUL(Tmp)
    return(tmpUTM)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
