\name{Prepare_WCGBTS_Extrapolation_Data_Fn}
\alias{Prepare_WCGBTS_Extrapolation_Data_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to load and process extrapolation grid for West Coast groundfish bottom trawl survey data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Prepare_WCGBTS_Extrapolation_Data_Fn(strata.limits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strata.limits}{
%%     ~~Describe \code{strata.limits} here~~
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
function (strata.limits) 
{
    data(extrapolation_data)
    Data_Extrap <- extrapolation_data
    Tmp = cbind(BEST_DEPTH_M = (-1000) * Data_Extrap[, "Depth_km"], 
        BEST_LAT_DD = Data_Extrap[, "Lat"], propInWCGBTS = Data_Extrap[, 
            "propInWCGBTS"])
    a_el = as.data.frame(matrix(NA, nrow = nrow(Data_Extrap), 
        ncol = nrow(strata.limits), dimnames = list(NULL, strata.limits[, 
            "STRATA"])))
    for (l in 1:ncol(a_el)) {
        a_el[, l] = apply(Tmp, MARGIN = 1, FUN = nwfscDeltaGLM::strata.fn, 
            Strata.df = strata.limits[l, ])
        a_el[, l] = ifelse(is.na(a_el[, l]), 0, 4 * Data_Extrap[, 
            "propInWCGBTS"])
    }
    tmpUTM = Convert_LL_to_UTM_Fn(Lon = Data_Extrap[, "Lon"], 
        Lat = Data_Extrap[, "Lat"])
    Data_Extrap = cbind(Data_Extrap, Include = (Data_Extrap[, 
        "Cowcod"] == 0 & Data_Extrap[, "Ngdc_m"] < (-35)))
    Data_Extrap[, c("E_km", "N_km")] = tmpUTM[, c("X", "Y")]
    Return = list(a_el = a_el, Data_Extrap = Data_Extrap)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
