

devtools::install_github("james-thorson/FishData")

# Load libraries
library(TMB)               # Can instead load library(TMBdebug)
library(SpatialDeltaGLMM)

# This is where all runs will be located
DateFile = paste0(tempdir(),'\\')

Region = "Eastern_Bering_Sea"
Database = FishData::scrape_data( species_set=10 )
species_set = unique( Database[,'Sci'] )

###############
# Settings
###############
Version = "geo_index_v4a"
Method = c("Grid", "Mesh")[2]
grid_size_km = 25
n_x = 100 # Number of stations
FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) # Structure for beta or epsilon over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
VesselConfig = c("Vessel"=0, "VesselYear"=0)
ObsModel = 2  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
Options = c(SD_site_density=0, SD_site_logdensity=0, Calculate_Range=1, Calculate_evenness=0, Calculate_effective_area=0)

# Decide on case-specific settings for use when calculating indices
strata.limits <- data.frame('STRATA'="All_areas")

# Save options for future records
Record = ThorsonUtilities::bundlelist( c("strata.limits","Region","Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","VesselConfig","ObsModel","Kmeans_Config","Options") )
save( Record, file=file.path(DateFile,"Record.RData"))

# Loop through species
for( sI in 2:length(species_set)){
  # Read or simulate trawl data
  Data_Geostat = Database[ which(Database[,'Sci']==species_set[sI]), ]
  Data_Geostat = ThorsonUtilities::rename_columns( Data_Geostat[,c('Wt','Year','Long','Lat')], newname=c('Catch_KG','Year','Lon','Lat') )
  Data_Geostat = cbind( Data_Geostat, 'AreaSwept_km2'=0.01, 'Vessel'=1 )

  # Get extrapolation data
  Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )

  # Calculate spatial information for SPDE mesh, strata areas, and AR1 process
  Spatial_List = Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=DateFile )
  Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )

  # Make TMB data list
  TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )

  # Make TMB object
  TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
  Obj = TmbList[["Obj"]]

  # Run model
  Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=FALSE )
  Report = Obj$report()
  Year_Set = min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])

  # Plot index
  Index = PlotIndex_Fn( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, strata_names=strata.limits[,1], use_biascorr=TRUE )

  # Plot center of gravity
  COG = Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Znames=colnames(TmbData$Z_xm), FileName_COG=paste0(DateFile,"center_of_gravity.png"))

  # save
  Save = list("Index"=Index$Table, "COG"=COG$Table)
  save( Save, file=paste0("C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/inst/extdata/",Region,"-",species_set[sI],".RData") )
}
