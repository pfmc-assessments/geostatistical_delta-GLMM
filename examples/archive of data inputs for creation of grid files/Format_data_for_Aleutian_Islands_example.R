
setwd("C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/")
library(mapdata)

# read file
aleutian_islands_grid = foreign::read.dbf( "examples/archive of data inputs for creation of grid files/aigrid2nmx2nm.dbf" )

# Remove unnecessary columns
aleutian_islands_grid = ThorsonUtilities::rename_columns( aleutian_islands_grid[,c('POINT_X','POINT_Y','STRATUM')], newname=c("Lon","Lat","Stratum"))
aleutian_islands_grid = cbind( aleutian_islands_grid, "Area_km2"=4 * 1.852^2)

# Plot
Which = which( aleutian_islands_grid[,'Stratum']!=0 )
maps::map( "worldHires" )
points( y=aleutian_islands_grid[Which,'Lat'], x=aleutian_islands_grid[Which,'Lon'])

# Convert previous TXT format to new RDA format
devtools::use_data( aleutian_islands_grid, pkg=getwd())

##############
# Check areas
##############

# Total
sum( aleutian_islands_grid[which( aleutian_islands_grid[,'Stratum']!=0 ), "Area_km2"] )

# By stratum
tapply( INDEX=aleutian_islands_grid$Stratum, FUN=sum, X=aleutian_islands_grid$Area_km2 )

##############
# Format example data
##############
setwd("C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/")

# Read data
AI_pacific_ocean_perch = read.csv( file="C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/data/aipopcpue9114.csv")

# Convert previous TXT format to new RDA format
devtools::use_data( AI_pacific_ocean_perch, pkg=getwd())

