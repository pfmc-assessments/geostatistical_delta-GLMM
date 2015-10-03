
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )

#####################
# Species
#####################

BC_pacific_cod_example <- read.csv( paste0(getwd(),"/examples/archive of data inputs for creation of grid files/BC coast/GFBIO_HS_QCS_SURVEYS_WITH_PCOD_POP_CATCH.csv") )
BC_pacific_cod_example <- BC_pacific_cod_example[,c('LAT','LON','Year','TOW.LENGTH..KM.','PCOD_WEIGHT')]
BC_pacific_cod_example[,'PCOD_WEIGHT'] = ifelse( is.na(BC_pacific_cod_example[,'PCOD_WEIGHT']), 0, BC_pacific_cod_example[,'PCOD_WEIGHT'])
devtools::use_data( BC_pacific_cod_example, pkg=getwd())

######################
# Grid
######################

bc_coast_grid <- read.csv( paste0(getwd(),"/examples/archive of data inputs for creation of grid files/BC coast/SPERA_GRIDS_5km.csv") )

bc_coast_grid <- cbind( bc_coast_grid, "Lat"=rowMeans(bc_coast_grid[,c("LAT1","LAT2","LAT3","LAT4")]))
bc_coast_grid <- cbind( bc_coast_grid, "Lon"=rowMeans(bc_coast_grid[,c("LON1","LON2","LON3","LON4")]))
bc_coast_grid <- cbind( bc_coast_grid[,c('Lat','Lon')], "Area_KM2"=25)
devtools::use_data( bc_coast_grid, pkg=getwd())

