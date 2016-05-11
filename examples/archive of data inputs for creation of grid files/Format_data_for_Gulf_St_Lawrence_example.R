
setwd("C:/Users/James.Thorson/Desktop/UW Hideaway/Collaborations/2016 -- Gulf of St Lawrence demo/")

gulf_of_st_lawrence_grid = read.csv( "sGSL grid 2nm sq.csv" )

library( maps )
map( "world", xlim=range(gulf_of_st_lawrence_grid$longitude), ylim=range(gulf_of_st_lawrence_grid$latitude) )
points( x=gulf_of_st_lawrence_grid$longitude, y=gulf_of_st_lawrence_grid$latitude)

setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
gulf_of_st_lawrence_grid = gulf_of_st_lawrence_grid[,2:4]
devtools::use_data( gulf_of_st_lawrence_grid, pkg=getwd())

#######################
# Prepare CPUE data
#######################

GSL_american_plaice = read.csv( "C:/Users/James.Thorson/Desktop/UW Hideaway/Collaborations/2016 -- Gulf of St Lawrence demo/RV American plaice_27APR2016_Thorson.csv" )
  GSL_american_plaice = GSL_american_plaice[ which(GSL_american_plaice[,'strat']>=415 & GSL_american_plaice[,'strat']<=439), ]
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
  devtools::use_data( GSL_american_plaice, pkg=getwd(), overwrite=TRUE)

