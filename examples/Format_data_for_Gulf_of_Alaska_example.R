
setwd("C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/")

# read file
gulf_of_alaska_grid = read.csv( "data/GOAgrid2nmx2nm.csv", header=TRUE )
#write.csv(eastern_bering_sea_grid, file="data/ebsgrid2nmx2nm.csv", row.names=FALSE )

# Remove unnecessary columns
gulf_of_alaska_grid = gulf_of_alaska_grid[,c('LONGITUDE.N.19.11','LATITUDE.N.19.11','STRATUM.N.14.0')]

# Rename columns
colnames(gulf_of_alaska_grid) = sapply(colnames(gulf_of_alaska_grid), FUN=function(char,...){ switch(char, ..., char)}, 'LONGITUDE.N.19.11'='Lon', 'LATITUDE.N.19.11'='Lat', 'STRATUM.N.14.0'='GOA_STRATUM')

# Add area column
gulf_of_alaska_grid = cbind( gulf_of_alaska_grid, 'Area_in_survey_km2'=ifelse(gulf_of_alaska_grid[,'GOA_STRATUM']!=0, 2^2*1.852^2, 0))

# Plot
plot( y=gulf_of_alaska_grid[,'Lat'], x=gulf_of_alaska_grid[,'Lon'])

# Convert previous TXT format to new RDA format
devtools::use_data( gulf_of_alaska_grid, pkg=getwd())

