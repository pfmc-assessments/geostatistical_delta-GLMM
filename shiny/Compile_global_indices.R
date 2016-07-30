#
########################
## Used to compile results globally
## Add comments to avoid auto-running by Shiny
## TO USE:  Uncomment first on entire script
#######################
#
#devtools::install_github("james-thorson/FishData")
#
## Database directory
#ResultsDir = "C:/Users/James.Thorson/Desktop/UW Hideaway/Website/FishViz/database_nx=250/"
#  dir.create(ResultsDir)
#
## Load libraries
#library(TMB)               # Can instead load library(TMBdebug)
#library(SpatialDeltaGLMM)
#
################
## Settings
################
#RegionSet = c("Eastern_Bering_Sea", "Gulf_of_Alaska", "Aleutian_Islands")
#Version = "geo_index_v4a"
#Method = c("Grid", "Mesh")[2]
#grid_size_km = 25
#n_x = 250  # Number of stations
#FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
#RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) # Structure for beta or epsilon over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
#VesselConfig = c("Vessel"=0, "VesselYear"=0)
#ObsModel = 2  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
#Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
#Options = c(SD_site_density=0, SD_site_logdensity=0, Calculate_Range=1, Calculate_evenness=0, Calculate_effective_area=0)
#
## Decide on case-specific settings for use when calculating indices
#strata.limits <- data.frame('STRATA'="All_areas")
#
################
## Run model
################
#
##for(rI in 1:length(RegionSet)){
#for(rI in 2:3){
#  ##### Loop through regions
#  Region = RegionSet[rI]
#  Database = FishData::scrape_data( region=Region, species_set=10, error_tol=0.01 )
#  species_set = unique( Database[,'Sci'] )
#
#  # This is where all runs will be located
#  TmpFile = paste0(tempdir(),'_Region=',Region,'\\')
#    dir.create(TmpFile)
#
#  ##### Loop through species
#  for( sI in 1:length(species_set)){
#    # Read or simulate trawl data
#    Data_Geostat = Database[ which(Database[,'Sci']==species_set[sI]), ]
#    Data_Geostat = ThorsonUtilities::rename_columns( Data_Geostat[,c('Wt','Year','Long','Lat')], newname=c('Catch_KG','Year','Lon','Lat') )
#    Data_Geostat = cbind( Data_Geostat, 'AreaSwept_km2'=0.01, 'Vessel'=1 )
#
#    # Get extrapolation data
#    Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
#
#    # Calculate spatial information for SPDE mesh, strata areas, and AR1 process
#    Spatial_List = Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=TmpFile )
#    Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )
#
#    # Make TMB data list
#    TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
#
#    # Make TMB object
#    TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=TmpFile, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
#    Obj = TmbList[["Obj"]]
#
#    # Run model
#    Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=NULL, bias.correct=FALSE )
#    Report = Obj$report()
#    Year_Set = min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])
#
#    # Plot index
#    Index = PlotIndex_Fn( DirName=TmpFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, strata_names=strata.limits[,1], use_biascorr=TRUE )
#
#    # Plot center of gravity
#    COG = Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Znames=colnames(TmbData$Z_xm), FileName_COG=paste0(TmpFile,"center_of_gravity.png"))
#
#    # save
#    Save = list("Index"=Index$Table[Index$Table[,'Year']%in%Data_Geostat$Year,], "COG"=COG$Table[COG$Table[,'Year']%in%Data_Geostat$Year,])
#    save( Save, file=paste0(ResultsDir,Region,"-",species_set[sI],".RData") )
#
#    # Plot maps for animation
#    MapDetails_List = MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
#    Dir = paste0(ResultsDir,"Image-",Region,"-",species_set[sI],"/")
#    dir.create( Dir )
#    for(tI in 1:TmbData$n_t){
#      if( any((TmbData$t_i+1)==tI) ){
#        ThorsonUtilities::save_fig( filename=paste0(Dir,Year_Set[tI]), width=4, height=4, res=200 )
#          Zlim = range( log(Report$D_xt) )
#          SpatialDeltaGLMM:::PlotMap_Fn(MappingDetails=list("world",NULL), Mat=log(Report$D_xt[,tI,drop=FALSE]), zlim=Zlim, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=NA, Format="", Year_Set=Year_Set[tI], outermargintext=c("",""), mar=c(0,0,2,0))
#        dev.off()
#      }
#    }
#  }
#}
