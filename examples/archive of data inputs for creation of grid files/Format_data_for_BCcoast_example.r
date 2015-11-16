
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

bc_coast1 <- read.csv( paste0(getwd(),"/examples/archive of data inputs for creation of grid files/BC coast/SPERA_Grid_2nm_With_Attributes--Grid.csv") )
bc_coast2 <- read.csv( paste0(getwd(),"/examples/archive of data inputs for creation of grid files/BC coast/SPERA_Grid_2nm_With_Attributes--Depth.csv") )
bc_coast3 <- read.csv( paste0(getwd(),"/examples/archive of data inputs for creation of grid files/BC coast/SPERA_Grid_2nm_With_Attributes--Temperature.csv") )
bc_coast4 <- read.csv( paste0(getwd(),"/examples/archive of data inputs for creation of grid files/BC coast/SPERA_Grid_2nm_With_Attributes--Survey_Areas.csv") )

# Combine
bc_coast_grid <- NULL
bc_coast_grid <- cbind( bc_coast_grid, "Lat"=rowMeans(bc_coast1[,c("LAT1","LAT2","LAT3","LAT4")]))
bc_coast_grid <- cbind( bc_coast_grid, "Lon"=rowMeans(bc_coast1[,c("LON1","LON2","LON3","LON4")]))
# Areas
bc_coast_grid <- cbind( bc_coast_grid, bc_coast4[match(bc_coast1$BLOCK_DESIGNATION,bc_coast4$BLOCK_DESIGNATION),c('SOG','WCVI','QCS','HS','WCHG')])
for(i in 1:5) bc_coast_grid[,c('SOG','WCVI','QCS','HS','WCHG')[i]] <- ifelse( is.na(bc_coast_grid[,c('SOG','WCVI','QCS','HS','WCHG')[i]]), 0, bc_coast_grid[,c('SOG','WCVI','QCS','HS','WCHG')[i]])
# Temperature
bc_coast_grid <- cbind( bc_coast_grid, bc_coast3[match(bc_coast1$BLOCK_DESIGNATION,bc_coast3$BLOCK_DESIGNATION),-1] )
# Depth
bc_coast_grid <- cbind( bc_coast_grid, bc_coast2[match(bc_coast1$BLOCK_DESIGNATION,bc_coast2$BLOCK_DESIGNATION),-1] )

# write
devtools::use_data( bc_coast_grid, pkg=getwd())

