
library( FishData )

# Obtain data
DF = survey_catch_rates( survey="WCGHL", species_set=Inf, add_zeros=FALSE )     #

#############
# Make extrapolation grid
#############

WCGHL_grid = unique( DF[,c('Lat','Long')] )
WCGHL_grid = cbind( WCGHL_grid, "Area_km2"=91.44^2*pi/1e6, "first_year_of_sampling"=NA)    # 91.44 meters search radius for each site
for(rI in 1:nrow(WCGHL_grid)){
  WCGHL_grid[rI,'first_year_of_sampling'] = min( DF[which(DF[,'Lat']==WCGHL_grid[rI,'Lat'] & DF[,'Long']==WCGHL_grid[rI,'Long']), 'Year'], na.rm=TRUE )
}
rownames(WCGHL_grid) = NULL

# Convert previous TXT format to new RDA format
devtools::use_data( WCGHL_grid, pkg="C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/")

#############
# Exploratory
#############

# Check number of sampled sites by year
Table = tapply( DF[,'Wt'], INDEX=list(DF[,'Lat'],DF[,'Year']), FUN=length )
colSums( Table/max(Table,na.rm=TRUE), na.rm=TRUE )

# Check encounter rates
Table = tapply( DF[,'Wt'], INDEX=DF[,'Year'], FUN=function(vec){mean(vec>0,na.rm=TRUE)} )

# Plot sites
library(maps)
library(mapdata)
map( "worldHires", xlim=range(WCGHL_grid[,'Long']), ylim=range(WCGHL_grid[,'Lat']) )
points( y=WCGHL_grid[,'Lat'], x=WCGHL_grid[,'Long'])

