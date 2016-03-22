
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
library( ThorsonUtilities )

new_zealand_grid <- read.csv( "C:/Users/James.Thorson/Desktop/New Zealand travel files/Collaborations/2016 -- New Zealand example/chatham_rise_grid/ChathamRiseGridEA_CHAT_n=500.csv" )

# Convert previous TXT format to new RDA format
devtools::use_data( new_zealand_grid, pkg=getwd())
