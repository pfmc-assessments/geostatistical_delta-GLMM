Description
=============

SpatialDeltaGLMM
* Is an R package for implementing a spatial delta-generalized linear mixed model (delta-GLMM) for use when standardizing fishery-independent index data for U.S. West Coast surveys.
* Has built in diagnostic functions and model-comparison tools
* Is intended to improve analysis speed, replicability, peer-review, and interpretation of index standardization methods
* Will eventually be improved to incorporate informative help files accessible via standard R commands.

Regions available in the [example script](https://github.com/nwfsc-assess/geostatistical_delta-GLMM/blob/master/examples/Example--simple.R): 
![alt text](https://github.com/nwfsc-assess/geostatistical_delta-GLMM/raw/master/examples/global_coverage.png "Logo Title Text 1")

Background
* This tool is designed to estimate spatial variation in density using fishery-independent data, with the goal of estimating total abundance for a target species in one or more years.  
* The model builds upon delta-generalized linear mixed modelling techniques (Thorson and Ward 2013,2014), which separately models the proportion of tows that catch at least one individual ("encounter probability") and catch rates for tows with at least one individual ("positive catch rates").  
* Submodels for encounter probability and positive catch rates always incorporate variation in density among years (as a fixed effect), and can incorporate variation among sampling vessels (as a random effect, Thorson and Ward 2014).  
* Each submodel can also estimate spatial variation (variation that is constant among years), and spatiotemporal variation (variation over space which differs among years).  
* Spatial and spatiotemporal variation are approximated as Gaussian Markov random fields (Thorson Skaug et al. In press), which imply that correlations in spatial variation decay as a function of distance.  
* The tool incorporates geometric anisotropy, i.e., differences and rotation of the direction of correlation, where correlations may decline faster inshore-offshore than alongshore (Thorson Shelton et al. In press). 

Installation Instructions
=============
This function depends on R version >=3.1.1 and a variety of other tools.

First, install the "devtools" package from CRAN

    # Install and load devtools package
    install.packages("devtools")
    library("devtools")

Second, please install the following:
* TMB (Template Model Builder): https://github.com/kaskr/adcomp
* INLA (integrated nested Laplace approximations): http://www.r-inla.org/download

Note: at the moment, TMB and INLA can be installed using the commands 

    # devtools command to get TMB from GitHub
    install_github("kaskr/adcomp/TMB") 
    # source script to get INLA from the web
    source("http://www.math.ntnu.no/inla/givemeINLA.R")  
    
Next, please install the geostatistical_delta-GLMM package from this GitHub repository using a function in the "devtools" package:

    # Install package
    install_github("nwfsc-assess/geostatistical_delta-GLMM", ref="3.3.0") 
    # Load package
    library(SpatialDeltaGLMM)

Or you can always use the development version

    # Install package
    install_github("nwfsc-assess/geostatistical_delta-GLMM") 

Known installation/usage issues
=============
none

Example code
=============
Please see examples folder for an example of how to run the model:
https://github.com/nwfsc-assess/geostatistical_delta-GLMM/blob/master/examples/Example--simple.R

This code illustrates how to loop through different default model configurations,
plot diagnostics for each model, and obtain the AIC for each model.

Please also read the [Guidelines for West Coast users](https://github.com/nwfsc-assess/geostatistical_delta-GLMM/wiki/West-Coast-Guidelines)
wiki page, which is a living document and will evolve over time as best practices
become apparent.


Further reading
=============

For more details regarding development and testing of this delta-GLMM software please see:
* Thorson, J.T., Skaug, H., Kristensen, K., Shelton, A.O., Ward, E.J., Harms, J., and Benante, J. In press. The importance of spatial models for estimating the strength of density dependence. Ecology, 96:1202–1212. doi: http://dx.doi.org/10.1890/14-0739.1. URL: http://www.esajournals.org/doi/abs/10.1890/14-0739.1
* Thorson, J. T., A. O. Shelton, E. J. Ward, and H. Skaug. In press. Geostatistical delta-generalized linear mixed models improve precision for estimated abundance indices for West Coast groundfishes. ICES Journal of Marine Science. URL: http://icesjms.oxfordjournals.org/content/early/2015/01/13/icesjms.fsu243.short?rss=1
* Shelton, A. O., J. T. Thorson, E. J. Ward, and B. E. Feist. in press. Spatial, semi-parametric models improve estimates of species abundance and distribution. Canadian Journal of Fisheries and Aquatic Sciences. URL: http://www.nrcresearchpress.com/doi/abs/10.1139/cjfas-2013-0508#.VMafDf7F_h4
* Thorson, J. T., I. J. Stewart, and A. E. Punt. 2012. Development and application of an agent-based model to evaluate methods for estimating relative abundance indices for shoaling fish such as Pacific rockfish (Sebastes spp.). ICES Journal of Marine Science 69:635–647. URL: http://icesjms.oxfordjournals.org/content/69/4/635
* Thorson, J. T., I. Stewart, and A. Punt. 2011. Accounting for fish shoals in single- and multi-species survey data using mixture distribution models. Canadian Journal of Fisheries and Aquatic Sciences 68:1681–1693. URL: http://www.nrcresearchpress.com/doi/abs/10.1139/f2011-086#.VMafcf7F_h4
