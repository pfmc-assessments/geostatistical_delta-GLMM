\name{Calc_Polygon_Areas_and_Polygons_Fn}
\alias{Calc_Polygon_Areas_and_Polygons_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate design matrix and polygon areas for a given set of knots
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Calc_Polygon_Areas_and_Polygons_Fn(loc_x, Data_Extrap, Covariates = "none", a_el = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loc_x}{
%%     ~~Describe \code{loc_x} here~~
}
  \item{Data_Extrap}{
%%     ~~Describe \code{Data_Extrap} here~~
}
  \item{Covariates}{
%%     ~~Describe \code{Covariates} here~~
}
  \item{a_el}{
%%     ~~Describe \code{a_el} here~~
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
function (loc_x, Data_Extrap, Covariates = "none", a_el = NULL) 
{
    if (is.null(a_el)) {
        a_el = rep(1, nrow(Data_Extrap))
    }
    NN_Extrap = nn2(data = loc_x[, c("E_km", "N_km")], query = Data_Extrap[, 
        c("E_km", "N_km")], k = 1)
    a_xl = matrix(NA, ncol = ncol(a_el), nrow = n_x, dimnames = list(NULL, 
        colnames(a_el)))
    for (l in 1:ncol(a_xl)) {
        a_xl[, l] = tapply(a_el[, l], INDEX = factor(NN_Extrap$nn.idx, 
            levels = 1:nrow(loc_x)), FUN = sum)
        a_xl[, l] = ifelse(is.na(a_xl[, l]), 0, a_xl[, l])
    }
    if (length(Covariates) == 1 && Covariates == "none") {
        X_xj = cbind(Dummy = rep(0, n_x))
    }
    else {
        X_xj = matrix(NA, ncol = length(Covariates), nrow = n_x)
        for (j in 1:ncol(X_xj)) {
            X_xj[, j] = tapply(Data_Extrap[, Covariates[j]], 
                INDEX = factor(NN_Extrap$nn.idx, levels = 1:nrow(loc_x)), 
                FUN = sum, na.rm = TRUE)
        }
    }
    Return = list(X_xj = X_xj, a_xl = a_xl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
