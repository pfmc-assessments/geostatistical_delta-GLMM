
setwd("C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/")

# read file
eastern_bering_sea_grid = read.csv( "data/ebsgrid2nmx2nm.csv", header=TRUE )
#write.csv(eastern_bering_sea_grid, file="data/ebsgrid2nmx2nm.csv", row.names=FALSE )

# Remove unnecessary columns
eastern_bering_sea_grid = eastern_bering_sea_grid[,c('POINT_X.N.19.11','POINT_Y.N.19.11','ELEVATION.N.10.0','MID_DEPTH.N.19.11','EBS_STRATU.N.5.0')]

# Rename columns
colnames(eastern_bering_sea_grid) = sapply(colnames(eastern_bering_sea_grid), FUN=function(char,...){ switch(char, ..., char)}, 'POINT_X.N.19.11'='Lon', 'POINT_Y.N.19.11'='Lat', 'ELEVATION.N.10.0'='Elevation', 'MID_DEPTH.N.19.11'='Mid_Depth', 'EBS_STRATU.N.5.0'='EBS_STRATUM')

# Plot
Which = which( eastern_bering_sea_grid[,'EBS_STRATUM']!=0 )
plot( y=eastern_bering_sea_grid[Which,'Lat'], x=eastern_bering_sea_grid[Which,'Lon'])

# Convert previous TXT format to new RDA format
devtools::use_data( eastern_bering_sea_grid, pkg=getwd())

##############
# Check areas
##############
# SqKm per stratum                  
77871.2 # 10  1 77,871          
41027.07  # 20  2 41,027      
94526.06  # 31  3 94,526     
8774.17 # 32  4 8,774         
62703.46  # 41  5 62,703          
24011.09  # 42  6 24,011          
21107.7 # 43  7 21,108          
38792.44  # 50  8 38,792          
88133.72  # 61  9 88,134          
6428.614  # 62  10  6,429         
20655.09  # 82  13  20,655          
11567.96  # 90  14  11,568                                                                      

# SqKm per stratum in this data
77871.2 # 10 = 1
41027.07 # 20 = 2
103300.2 # 31+32= 3
107822.2 # 41+ 42+ 43=4
38792.44 # 50 = 5
94562.33 # 61+62= 6
32223.05 # 82+ 90 = 7

# Looks right!
table( eastern_bering_sea_grid$EBS_STRATUM) * 4 * 1.852^2
