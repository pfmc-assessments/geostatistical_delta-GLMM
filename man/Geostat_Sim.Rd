\name{Geostat_Sim}
\alias{Geostat_Sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Simulates spatiotemporal survey data }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Geostat_Sim(Sim_Settings, MakePlot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sim_Settings}{
%%     ~~Describe \code{Sim_Settings} here~~
}
  \item{MakePlot}{
%%     ~~Describe \code{MakePlot} here~~
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
function (Sim_Settings, MakePlot = TRUE) 
{
    model_O1 = RMgauss(var = Sim_Settings[["SigmaO1"]]^2, scale = Sim_Settings[["Range1"]])
    model_E1 = RMgauss(var = Sim_Settings[["SigmaE1"]]^2, scale = Sim_Settings[["Range1"]])
    model_O2 = RMgauss(var = Sim_Settings[["SigmaO2"]]^2, scale = Sim_Settings[["Range2"]])
    model_E2 = RMgauss(var = Sim_Settings[["SigmaE2"]]^2, scale = Sim_Settings[["Range2"]])
    s_i = sample(1:nrow(Data_Extrap), size = Sim_Settings[["Nyears"]] * 
        Sim_Settings[["Nsamp_per_year"]])
    loc_i = Data_Extrap[s_i, c("E_km", "N_km", "Lat", "Lon")]
    t_i = rep(1:Sim_Settings[["Nyears"]], each = Sim_Settings[["Nsamp_per_year"]])
    v_i = rep(rep(1:4, each = Sim_Settings[["Nsamp_per_year"]]/4), 
        Sim_Settings[["Nyears"]])
    O1_i = RFsimulate(model = model_O1, x = loc_i[, "E_km"], 
        y = loc_i[, "N_km"])@data[, 1]
    O2_i = RFsimulate(model = model_O2, x = loc_i[, "E_km"], 
        y = loc_i[, "N_km"])@data[, 1]
    E1_i = E2_i = rep(NA, Sim_Settings[["Nsamp_per_year"]] * 
        Sim_Settings[["Nyears"]])
    for (t in 1:Sim_Settings[["Nyears"]]) {
        E1_i[which(t_i == t)] = RFsimulate(model = model_E1, 
            x = loc_i[which(t_i == t), "E_km"], y = loc_i[which(t_i == 
                t), "N_km"])@data[, 1]
        E2_i[which(t_i == t)] = RFsimulate(model = model_E2, 
            x = loc_i[which(t_i == t), "E_km"], y = loc_i[which(t_i == 
                t), "N_km"])@data[, 1]
    }
    X_i = Data_Extrap[s_i, c("Depth_km", "Depth_km2", "Dist_sqrtkm")]
    Vessel_vyc = array(rnorm(n = 4 * Sim_Settings[["Nyears"]] * 
        2, mean = 0, sd = Sim_Settings[["SigmaVY1"]]), dim = c(4, 
        Sim_Settings[["Nyears"]], 2))
    P1_i = O1_i + E1_i + as.matrix(X_i) \%*\% unlist(Sim_Settings[c("Depth_km", 
        "Depth_km2", "Dist_sqrtkm")])
    R1_i = plogis(P1_i + Vessel_vyc[v_i[which(t_i == t)], t, 
        1])
    P2_i = O1_i + E1_i + as.matrix(X_i) \%*\% unlist(Sim_Settings[c("Depth_km", 
        "Depth_km2", "Dist_sqrtkm")])
    R2_i = exp(P2_i + Vessel_vyc[v_i[which(t_i == t)], t, 2])
    CPUE_i = rlnorm(n = length(R2_i), meanlog = log(R2_i), sdlog = Sim_Settings[["SigmaM"]]) * 
        rbinom(n = length(R1_i), size = 1, prob = R1_i)
    Data_Geostat = cbind(Catch_KG = CPUE_i, Year = t_i, Vessel = v_i, 
        AreaSwept_km2 = 1/100, Lat = loc_i[, "Lat"], Lon = loc_i[, 
            "Lon"])
    if (MakePlot == TRUE) {
        f = function(Num) ((Num) - min((Num), na.rm = TRUE))/diff(range((Num), 
            na.rm = TRUE))
        Col = colorRampPalette(colors = c("blue", "grey", "red"))
        Xlim = c(-126, -117)
        Ylim = c(32, 49)
        MapSizeRatio = c(`Height(in)` = 4, `Width(in)` = 2)
        for (RespI in 1:5) {
            Mat = matrix(NA, ncol = Sim_Settings[["Nyears"]], 
                nrow = nrow(Data_Extrap))
            for (t in 1:Sim_Settings[["Nyears"]]) {
                NN_Extrap = nn2(data = loc_i[which(t_i == t), 
                  c("E_km", "N_km")], query = Data_Extrap[, c("E_km", 
                  "N_km")], k = 1)
                if (RespI == 1) 
                  Mat[, t] = (R1_i[which(t_i == t)])[NN_Extrap$nn.idx]
                if (RespI == 2) 
                  Mat[, t] = (R2_i[which(t_i == t)])[NN_Extrap$nn.idx]
                if (RespI == 3) 
                  Mat[, t] = (R1_i[which(t_i == t)] * R2_i[which(t_i == 
                    t)])[NN_Extrap$nn.idx]
                if (RespI == 4) 
                  Mat[, t] = (log(R2_i[which(t_i == t)] + quantile(R2_i[which(t_i == 
                    t)], 0.25)))[NN_Extrap$nn.idx]
                if (RespI == 5) 
                  Mat[, t] = (log(R1_i[which(t_i == t)] * R2_i[which(t_i == 
                    t)] + quantile(R1_i[which(t_i == t)] * R2_i[which(t_i == 
                    t)], 0.25)))[NN_Extrap$nn.idx]
                if (RespI == 3) 
                  True_Index = colSums(Mat)
            }
            png(file = paste(DateFile, Species, "_True_", switch(RespI, 
                "Pres", "Pos", "Dens", "Pos_Rescaled", "Dens_Rescaled"), 
                ".png", sep = ""), width = 5 * MapSizeRatio["Width(in)"], 
                height = 2 * MapSizeRatio["Height(in)"], res = 200, 
                units = "in")
            par(mfrow = c(2, 5), mar = c(2, 2, 0, 0))
            for (t in 1:Sim_Settings[["Nyears"]]) {
                map("worldHires", ylim = Ylim, xlim = Xlim, col = "grey90", 
                  fill = T, main = "", mar = c(0, 0, 2.5, 0), 
                  interior = TRUE)
                points(x = Data_Extrap[, "Lon"], y = Data_Extrap[, 
                  "Lat"], col = Col(n = 10)[ceiling(f(Mat)[, 
                  t] * 9) + 1], cex = 0.01)
            }
            dev.off()
        }
    }
    Return = list(Data_Geostat = Data_Geostat, True_Index = True_Index)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
