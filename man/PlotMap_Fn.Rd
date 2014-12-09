\name{PlotMap_Fn}
\alias{PlotMap_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots surfaces after parameter estimation}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMap_Fn(MappingDetails, Report, MapSizeRatio, Xlim, Ylim, FileName, Year_Set)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MappingDetails}{
%%     ~~Describe \code{MappingDetails} here~~
}
  \item{Report}{
%%     ~~Describe \code{Report} here~~
}
  \item{MapSizeRatio}{
%%     ~~Describe \code{MapSizeRatio} here~~
}
  \item{Xlim}{
%%     ~~Describe \code{Xlim} here~~
}
  \item{Ylim}{
%%     ~~Describe \code{Ylim} here~~
}
  \item{FileName}{
%%     ~~Describe \code{FileName} here~~
}
  \item{Year_Set}{
%%     ~~Describe \code{Year_Set} here~~
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
function (MappingDetails, Report, MapSizeRatio, Xlim, Ylim, FileName, 
    Year_Set) 
{
    D_it = Report$D_xt[NN_Extrap$nn.idx, ]
    R1_it = Report$R1_xt[NN_Extrap$nn.idx, ]
    R2_it = Report$R2_xt[NN_Extrap$nn.idx, ]
    f = function(Num) ((Num) - min((Num), na.rm = TRUE))/diff(range((Num), 
        na.rm = TRUE))
    Col = colorRampPalette(colors = c("darkblue", "blue", "lightblue", 
        "lightgreen", "yellow", "orange", "red"))
    for (RespI in 1:5) {
        if (RespI == 1) 
            Mat = R1_it
        if (RespI == 2) {
            Mat = log(R2_it + quantile(R2_it, 0.01))
            Mat = ifelse(Mat < (-5), -5, Mat)
        }
        if (RespI == 3) {
            Mat = log(D_it + quantile(D_it, 0.01))
            Mat = ifelse(Mat < (-5), -5, Mat)
        }
        if (RespI == 4) 
            Mat = log(R2_it + quantile(R2_it, 0.25))
        if (RespI == 5) 
            Mat = log(D_it + quantile(D_it, 0.25))
        png(file = paste0(FileName, switch(RespI, "Pres", "Pos", 
            "Dens", "Pos_Rescaled", "Dens_Rescaled"), ".png"), 
            width = 5 * MapSizeRatio["Width(in)"], height = 2 * 
                MapSizeRatio["Height(in)"], res = 400, units = "in")
        par(mfrow = c(2, 6), oma = c(2, 2, 0, 0))
        for (t in 1:length(Year_Set)) {
            Which = which(Data_Extrap[, "Include"] == TRUE)
            map(MappingDetails[[1]], MappingDetails[[2]], ylim = Ylim, 
                xlim = Xlim, col = "grey90", fill = TRUE, main = "", 
                mar = c(0, 0, 2, 0), interior = TRUE)
            points(x = Data_Extrap[Which, "Lon"], y = Data_Extrap[Which, 
                "Lat"], col = Col(n = 50)[ceiling(f(Mat[Which, 
                ])[, t] * 49) + 1], cex = 0.01)
            title(Year_Set[t], line = 0.1, cex.main = 1.5)
            box()
        }
        mtext(side = 1, outer = TRUE, "Longitude", cex = 1.75)
        mtext(side = 2, outer = TRUE, "Latitude", cex = 1.75)
        dev.off()
        png(file = paste0(FileName, switch(RespI, "Pres", "Pos", 
            "Dens", "Pos_Rescaled", "Dens_Rescaled"), "_Legend.png", 
            sep = ""), width = 1, height = 2 * MapSizeRatio["Height(in)"], 
            res = 200, units = "in")
        Heatmap_Legend(colvec = Col(n = 50), heatrange = range(Mat), 
            margintext = switch(RespI, "Encounter probability", 
                "log(Positive catch rate)", expression(paste("log Density, log(kg. / ", 
                  km^2, ")", sep = "")), NULL, NULL))
        dev.off()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
