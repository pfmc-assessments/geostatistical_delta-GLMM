Description
=============

SpatialDeltaGLMM

* Is an R package for implementing a spatial delta-generalized linear mixed model (delta-GLMM) for use when standardizing fishery-independent index data for U.S. West Coast surveys.
* Has built in diagnostic functions and model-comparison tools
* Is intended to improve analysis speed, replicability, peer-review, and interpretation of index standardization methods

Instructions
=============
First, please install JAGS (Just Another Gibbs Sampler) here: http://mcmc-jags.sourceforge.net/

Next, please use R version >=3.1.1 and install the package:


    # Install package
    install.packages("devtools")
    library("devtools")
    install_github("nwfsc-assess/geostatistical_delta-GLMM")
    # Load package
    library(SpatialDeltaGLMM)

Please see examples folder for an example of how to run the model:
https://github.com/nwfsc-assess/geostatistical_delta-GLMM/blob/master/examples/example_for_NWFSC_shelf-slope_data.R

Known installation/usage issues
=============
none

Further reading
=============

For more details regarding development and testing of this delta-GLMM software please see:
* Shelton, A. O., J. T. Thorson, E. J. Ward, and B. E. Feist. in press. Spatial, semi-parametric models improve estimates of species abundance and distribution. Canadian Journal of Fisheries and Aquatic Sciences.
* Thorson, J. T., and E. J. Ward. 2014. Accounting for vessel effects when standardizing catch rates from cooperative surveys. Fisheries Research 155:168–176.
* Thorson, J. T., and E. Ward. 2013. Accounting for space-time interactions in index standardization models. Fisheries Research 147:426–433.
* Thorson, J. T., I. J. Stewart, and A. E. Punt. 2012. Development and application of an agent-based model to evaluate methods for estimating relative abundance indices for shoaling fish such as Pacific rockfish (Sebastes spp.). ICES Journal of Marine Science 69:635–647.
* Thorson, J. T., I. Stewart, and A. Punt. 2011. Accounting for fish shoals in single- and multi-species survey data using mixture distribution models. Canadian Journal of Fisheries and Aquatic Sciences 68:1681–1693.
