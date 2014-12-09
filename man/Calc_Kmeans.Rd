\name{Calc_Kmeans}
\alias{Calc_Kmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculates locations for knots used in piecewise constant approximation}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Calc_Kmeans(n_x, Kmeans_Config, Data_Geostat, Data_Extrap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_x}{
%%     ~~Describe \code{n_x} here~~
}
  \item{Kmeans_Config}{
%%     ~~Describe \code{Kmeans_Config} here~~
}
  \item{Data_Geostat}{
%%     ~~Describe \code{Data_Geostat} here~~
}
  \item{Data_Extrap}{
%%     ~~Describe \code{Data_Extrap} here~~
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
function (n_x, Kmeans_Config, Data_Geostat, Data_Extrap) 
{
    old.options <- options()
    options(warn = -1)
    on.exit(options(old.options))
    if (paste0("Kmeans-", n_x, ".RData") \%in\% list.files(getwd())) {
        load(file = paste(DateFile, "Kmeans.RData", sep = ""))
    }
    else {
        Kmeans = list(tot.withinss = Inf)
        for (i in 1:Kmeans_Config[["nstart"]]) {
            if (Kmeans_Config[["Locs"]] == "Samples") {
                Tmp = kmeans(x = Data_Geostat[, c("E_km", "N_km")], 
                  centers = n_x, iter.max = Kmeans_Config[["iter.max"]], 
                  nstart = 1, trace = 0)
            }
            if (Kmeans_Config[["Locs"]] == "Domain") {
                Tmp = kmeans(x = Data_Extrap[, c("E_km", "N_km")], 
                  centers = n_x, iter.max = Kmeans_Config[["iter.max"]], 
                  nstart = 1, trace = 0)
            }
            print(paste0("Num=", i, " Current_Best=", round(Kmeans$tot.withinss, 
                1), " New=", round(Tmp$tot.withinss, 1)))
            if (Tmp$tot.withinss < Kmeans$tot.withinss) {
                Kmeans = Tmp
            }
        }
    }
    return(Kmeans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
