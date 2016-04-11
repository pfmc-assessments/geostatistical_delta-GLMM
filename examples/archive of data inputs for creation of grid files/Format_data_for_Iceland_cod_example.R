
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
library( ThorsonUtilities )

load( "C:/Users/James.Thorson/Desktop/UW Hideaway/Collaborations/2016 -- Iceland index demo/Data_Species_1.RData" )
iceland_cod = Data_Species

# Convert previous TXT format to new RDA format
devtools::use_data( iceland_cod, pkg=getwd())
