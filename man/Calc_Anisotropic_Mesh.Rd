\name{Calc_Anisotropic_Mesh}
\alias{Calc_Anisotropic_Mesh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculates mesh and generates elements for anisotropic random fields }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Calc_Anisotropic_Mesh(loc_x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loc_x}{
%%     ~~Describe \code{loc_x} here~~
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
function (loc_x) 
{
    mesh = inla.mesh.create(loc_x, plot.delay = NULL, refine = FALSE)
    spde = inla.spde2.matern(mesh, alpha = 2)
    Dset = 1:2
    TV = mesh$graph$tv
    V0 = mesh$loc[TV[, 1], Dset]
    V1 = mesh$loc[TV[, 2], Dset]
    V2 = mesh$loc[TV[, 3], Dset]
    E0 = V2 - V1
    E1 = V0 - V2
    E2 = V1 - V0
    TmpFn = function(Vec1, Vec2) abs(det(rbind(Vec1, Vec2)))
    Tri_Area = rep(NA, nrow(E0))
    for (i in 1:length(Tri_Area)) Tri_Area[i] = TmpFn(E0[i, ], 
        E1[i, ])/2
    Return = list(Tri_Area = Tri_Area, TV = TV, E0 = E0, E1 = E1, 
        E2 = E2, mesh = mesh, spde = spde)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
