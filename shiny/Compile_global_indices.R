#
########################
## Used to compile results globally
## Add comments to avoid auto-running by Shiny
## TO USE:  Uncomment first on entire script
#######################
#
#if( !"FishData" %in% installed.packages()) devtools::install_github("james-thorson/FishData")
#if( !"taxize" %in% installed.packages()) install_packages("taxize") # sci2comm
#
## Database directory
#ResultsDir = "C:/Users/James.Thorson/Desktop/UW Hideaway/Website/FishViz/database_nx=1000/"
#  dir.create(ResultsDir)
#
## Load libraries
#library(TMB)               # Can instead load library(TMBdebug)
#library(SpatialDeltaGLMM)
#
################
## Settings
################
#RegionSet = c("Eastern_Bering_Sea", "Gulf_of_Alaska", "Aleutian_Islands", "New_Zealand", "California_current", "Gulf_of_St_Lawrence", "British_Columbia", "Northwest_Atlantic", "South_Africa")
#Version = "geo_index_v4a"
#Method = c("Grid", "Mesh")[2]
#grid_size_km = 25
#n_x = 1000  # Number of stations
#FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence; #Epsilon=Spatio-temporal; #Omega=Spatial
#RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) # Structure for beta or epsilon over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
#VesselConfig = c("Vessel"=0, "VesselYear"=0)
#ObsModel = 2  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
#Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )     # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
#Options = c(SD_site_density=0, SD_site_logdensity=0, Calculate_Range=1, Calculate_evenness=0, Calculate_effective_area=1)
#
## Decide on case-specific settings for use when calculating indices
#strata.limits <- data.frame('STRATA'="All_areas")
#
################
## Run model
################
#
#NamesDF = NULL
#
###### Loop through regions
#for(rI in 1:length(RegionSet)){
##for(rI in 1:3){
#  # Which region
#  Region = RegionSet[rI]
#
#  # This is where all runs will be located
#  RegionDir = paste0(ResultsDir,'Region=',Region,'\\')
#    dir.create(RegionDir)
#
#  # Regions with a public API
#  if( Region %in% c("Eastern_Bering_Sea", "Gulf_of_Alaska", "Aleutian_Islands")){
#    Database = FishData::scrape_data( region=Region, species_set=20, error_tol=0.01 )
#    species_set = unique( Database[,'Sci'] )
#    Database = ThorsonUtilities::rename_columns( Database[,c('Sci','Wt','Year','Long','Lat')], newname=c('Sci','Catch_KG','Year','Lon','Lat') )
#    Database = cbind( Database, 'AreaSwept_km2'=0.01, 'Vessel'=1 )
#  }else{
#    if( Region=="California_current"){
#      data( WCGBTS_Canary_example, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Catch_KG"=WCGBTS_Canary_example[,'HAUL_WT_KG'], "Year"=as.numeric(sapply(WCGBTS_Canary_example[,'PROJECT_CYCLE'],FUN=function(Char){strsplit(as.character(Char)," ")[[1]][2]})), "Vessel"=WCGBTS_Canary_example[,"VESSEL"], "AreaSwept_km2"=WCGBTS_Canary_example[,"AREA_SWEPT_HA"]/1e2, "Lat"=WCGBTS_Canary_example[,'BEST_LAT_DD'], "Lon"=WCGBTS_Canary_example[,'BEST_LON_DD'], "Pass"=WCGBTS_Canary_example[,'PASS']-1.5)
#      species_set = "canary_rockfish"
#    }
#    if( Region=="British_Columbia"){
#      data( BC_pacific_cod_example, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Catch_KG"=BC_pacific_cod_example[,'PCOD_WEIGHT'], "Year"=BC_pacific_cod_example[,'Year'], "Vessel"="missing", "AreaSwept_km2"=BC_pacific_cod_example[,'TOW.LENGTH..KM.']/100, "Lat"=BC_pacific_cod_example[,'LAT'], "Lon"=BC_pacific_cod_example[,'LON'], "Pass"=0)
#      species_set = "pacific_cod"
#    }
#    if( Region=="Gulf_of_St_Lawrence"){
#      data( GSL_american_plaice, package="SpatialDeltaGLMM" )
#      Print_Message( "GSL_american_plaice" )
#      Database = data.frame( "Year"=GSL_american_plaice[,'year'], "Lat"=GSL_american_plaice[,'latitude'], "Lon"=GSL_american_plaice[,'longitude'], "Vessel"="missing", "AreaSwept_km2"=GSL_american_plaice[,'swept'], "Catch_KG"=GSL_american_plaice[,'biomass']*GSL_american_plaice[,'vstd'] )
#      species_set = "plaice"
#    }
#    if( Region=="Northwest_Atlantic"){
#      data( georges_bank_haddock_spring, package="SpatialDeltaGLMM" )         # standardized area swept = 0.0112 nm^2 = 0.0112*1.852^2 km^2
#      Print_Message( "GB_haddock" )
#      Database = data.frame( "Catch_KG"=georges_bank_haddock_spring[,'CATCH_WT_CAL'], "Year"=georges_bank_haddock_spring[,'YEAR'], "Vessel"="missing", "AreaSwept_km2"=0.0112*1.852^2, "Lat"=georges_bank_haddock_spring[,'LATITUDE'], "Lon"=georges_bank_haddock_spring[,'LONGITUDE'])
#      species_set = "haddock"
#    }
#    if( Region=="South_Africa"){
#      data( south_africa_westcoast_jacopever, package="SpatialDeltaGLMM" )         # standardized area swept = 0.0112 nm^2 = 0.0112*1.852^2 km^2
#      Database = data.frame( "Catch_KG"=south_africa_westcoast_jacopever[,'HELDAC'], "Year"=south_africa_westcoast_jacopever[,'Year'], "Vessel"="missing", "AreaSwept_km2"=south_africa_westcoast_jacopever[,'area_swept_nm2']*1.852^2, "Lat"=south_africa_westcoast_jacopever[,'cen_lat'], "Lon"=south_africa_westcoast_jacopever[,'cen_long'])
#      species_set = "jacopever"
#    }
#    if( Region=="Iceland"){
#      # WARNING:  This data set has not undergone much evaluation for spatio-temporal analysis
#      data( iceland_cod, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Catch_KG"=iceland_cod[,'Catch_b'], "Year"=iceland_cod[,'year'], "Vessel"=1, "AreaSwept_km2"=iceland_cod[,'towlength'], "Lat"=iceland_cod[,'lat1'], "Lon"=iceland_cod[,'lon1'])
#      species_set = "cod"
#    }
#    if( Region=="New_Zealand"){
#      data( chatham_rise_hake, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Catch_KG"=chatham_rise_hake[,'Hake_kg_per_km2'], "Year"=chatham_rise_hake[,'Year'], "Vessel"=1, "AreaSwept_km2"=1, "Lat"=chatham_rise_hake[,'Lat'], "Lon"=chatham_rise_hake[,'Lon'])
#      species_set = "hake"
#    }
#    Database = na.omit( Database )
#  }
#
#  ##### Loop through species
#  for( sI in 1:length(species_set)){
#    # Read or simulate trawl data
#    if( 'Sci' %in% names(Database)){
#      Data_Geostat = Database[ which(Database[,'Sci']==species_set[sI]), ]
#    }else{
#      Data_Geostat = Database
#    }
#
#    # Get common names
#    if(FALSE){
#      Itis = taxize::sci2comm(sciname=species_set[sI], db="itis", simplify=TRUE)
#      NamesDF = rbind( NamesDF, c("Region"=Region, "Sci"=as.character(species_set[sI]), "Common"=rev(Itis[[1]])[1]) )
#    }
#
#    # Get extrapolation data
#    if( Region %in% c("British_Columbia","South_Africa","Iceland","Northwest_Atlantic") ){
#      if( Region == "British_Columbia" ){
#        Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, strata_to_use=c("HS","QCS") )
#      }
#      if( Region == "South_Africa" ){
#        Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, region="west_coast" )
#      }
#      if( Region == "Iceland" ){
#        Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, observations_LL=Data_Geostat[,c('Lat','Lon')], maximum_distance_from_sample=15 )
#      }
#      if( Region == "Northwest_Atlantic" ){
#        Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=list('Georges_Bank'=c(1130,1140,1150,1160,1170,1180,1190,1200,1210,1220,1230,1240,1250,1290,1300)) )
#      }
#    }else{
#      Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
#    }
#
#    # Calculate spatial information for SPDE mesh, strata areas, and AR1 process
#    Spatial_List = Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=RegionDir )
#    Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )
#
#    # Make TMB data list
#    TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
#
#    # Make TMB object
#    TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=RegionDir, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
#    Obj = TmbList[["Obj"]]
#
#    # Run model
#    Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=NULL, bias.correct=FALSE )
#    Report = Obj$report()
#    Year_Set = min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])
#    dyn.unload(paste0(RegionDir,"/",TMB::dynlib(Version)))
#
#    # Plot index
#    Index = PlotIndex_Fn( DirName=RegionDir, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, strata_names=strata.limits[,1], use_biascorr=TRUE )
#
#    # Plot center of gravity
#    COG = Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Znames=colnames(TmbData$Z_xm), FileName_COG=paste0(RegionDir,"center_of_gravity.png"))
#
#    # save
#    Save = list("Index"=Index$Table[Index$Table[,'Year']%in%Data_Geostat$Year,], "COG"=COG$Table[COG$Table[,'Year']%in%Data_Geostat$Year,])
#    save( Save, file=paste0(ResultsDir,Region,"-",species_set[sI],".RData") )
#
#    # Plot maps for animation   # SpatialDeltaGLMM::
#    MapDetails_List = MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
#    Dir = paste0(ResultsDir,"Image-",Region,"-",species_set[sI],"/")
#    dir.create( Dir )
#    for(tI in 1:TmbData$n_t){
#      if( any((TmbData$t_i+1)==tI) ){
#        ThorsonUtilities::save_fig( filename=paste0(Dir,Year_Set[tI]), width=MapDetails_List$MapSizeRatio['Width(in)'], height=MapDetails_List$MapSizeRatio['Height(in)'], res=200 )
#          Zlim = range( log(Report$D_xt) )
#          # MappingDetails=MapDetails_List$MappingDetails; Mat=log(Report$D_xt[,tI,drop=FALSE]); zlim=Zlim; PlotDF=MapDetails_List[["PlotDF"]]; MapSizeRatio=MapDetails_List[["MapSizeRatio"]]; Xlim=MapDetails_List[["Xlim"]]; Ylim=MapDetails_List[["Ylim"]]; FileName=NA; Format=""; Year_Set=Year_Set[tI]; outermargintext=c("",""); mar=c(0,0,2,0); Cex=0.5
#          SpatialDeltaGLMM:::PlotMap_Fn(MappingDetails=MapDetails_List$MappingDetails, Mat=log(Report$D_xt[,tI,drop=FALSE]), zlim=Zlim, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=NA, Format="", Year_Set=Year_Set[tI], outermargintext=c("",""), mar=c(2,2,2,0), Cex=switch(Region, "Northwest_Atlantic"=2, 0.5))
#          axis(1); axis(2)
#        dev.off()
#      }
#    }
#  }
#}
