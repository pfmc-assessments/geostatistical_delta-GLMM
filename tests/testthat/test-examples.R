
# Tutorial: http://r-pkgs.had.co.nz/tests.html
# And example see: https://github.com/ss3sim/ss3sim/tree/master/tests/testthat
context("Testing examples")

# Eastern Bering Sea pollcok
# Test default settings (mesh, spatio-temporal variation, gamma distribution
test_that("Eastern Bering Sea pollock is working ", {
  # Prepping
  test_path = file.path(example_path,"EBS_pollock")
  load( file.path(test_path,"opt.RData") )
  load( file.path(test_path,"Record.RData") )
  attach(Record)
  on.exit( detach(Record) )
  # Run model
  data( EBS_pollock_data, package="SpatialDeltaGLMM" )
  Data_Geostat = data.frame( "Catch_KG"=EBS_pollock_data[,'catch'], "Year"=EBS_pollock_data[,'year'], "Vessel"="missing", "AreaSwept_km2"=0.01, "Lat"=EBS_pollock_data[,'lat'], "Lon"=EBS_pollock_data[,'long'], "Pass"=0)
  Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
  Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=test_path )
  Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )
  TmbData = SpatialDeltaGLMM::Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method )
  TmbList = SpatialDeltaGLMM::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=test_path, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
  on.exit( dyn.unload(paste0(test_path,"/",TMB::dynlib(Record$Version))), add=TRUE )
  Opt = TMBhelper::Optimize( obj=TmbList[["Obj"]], getsd=FALSE, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]] )  # , rel.tol=1e-20
  # Comparisons
  expect_equal( Opt$par, opt$par, tolerance=1e-3 )
  expect_equal( Opt$objective, opt$objective, tolerance=1e-3 )
})

# Chatham rise
# Test default + using a grid
test_that("Chatham Rise hake is working ", {
  # Prepping
  test_path = file.path(example_path,"chatham_rise_hake")
  load( file.path(test_path,"opt.RData") )
  load( file.path(test_path,"Record.RData") )
  attach(Record)
  on.exit( detach(Record) )
  # Run model
  data( chatham_rise_hake, package="SpatialDeltaGLMM" )
  Data_Geostat = data.frame( "Catch_KG"=chatham_rise_hake[,'Hake_kg_per_km2'], "Year"=chatham_rise_hake[,'Year'], "Vessel"=1, "AreaSwept_km2"=1, "Lat"=chatham_rise_hake[,'Lat'], "Lon"=chatham_rise_hake[,'Lon'])
  Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
  Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=test_path )
  Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )
  TmbData = SpatialDeltaGLMM::Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method )
  TmbList = SpatialDeltaGLMM::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=test_path, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
  on.exit( dyn.unload(paste0(test_path,"/",TMB::dynlib(Record$Version))), add=TRUE )
  Opt = TMBhelper::Optimize( obj=TmbList[["Obj"]], getsd=FALSE, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]] )  # , rel.tol=1e-20
  # Comparisons
  expect_equal( Opt$par, opt$par, tolerance=1e-3 )
  expect_equal( Opt$objective, opt$objective, tolerance=1e-3 )
})

# California Current
# Test default + including vessel-year effects
test_that("West Coast groundfish bottom trawl survey, canary rockfish is working ", {
  # Prepping
  test_path = file.path(example_path,"WCGBTS_canary")
  load( file.path(test_path,"opt.RData") )
  load( file.path(test_path,"Record.RData") )
  attach(Record)
  on.exit( detach(Record) )
  # Run model
  data( WCGBTS_Canary_example, package="SpatialDeltaGLMM" )
  Data_Geostat = data.frame( "Catch_KG"=WCGBTS_Canary_example[,'HAUL_WT_KG'], "Year"=as.numeric(sapply(WCGBTS_Canary_example[,'PROJECT_CYCLE'],FUN=function(Char){strsplit(as.character(Char)," ")[[1]][2]})), "Vessel"=WCGBTS_Canary_example[,"VESSEL"], "AreaSwept_km2"=WCGBTS_Canary_example[,"AREA_SWEPT_HA"]/1e2, "Lat"=WCGBTS_Canary_example[,'BEST_LAT_DD'], "Lon"=WCGBTS_Canary_example[,'BEST_LON_DD'], "Pass"=WCGBTS_Canary_example[,'PASS']-1.5)
  Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
  Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=test_path )
  Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )
  TmbData = SpatialDeltaGLMM::Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method )
  TmbList = SpatialDeltaGLMM::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=test_path, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
  on.exit( dyn.unload(paste0(test_path,"/",TMB::dynlib(Record$Version))), add=TRUE )
  Opt = TMBhelper::Optimize( obj=TmbList[["Obj"]], getsd=FALSE, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]] )  # , rel.tol=1e-20
  # Comparisons
  expect_equal( Opt$par, opt$par, tolerance=1e-3 )
  expect_equal( Opt$objective, opt$objective, tolerance=1e-3 )
})

# Aleutian Islands
# Test default + Random-walk on spatio-temporal variation
test_that("Aleutian Islands groundfish bottom trawl survey, POP is working ", {
  # Prepping
  test_path = file.path(example_path,"AI_POP")
  load( file.path(test_path,"opt.RData") )
  load( file.path(test_path,"Record.RData") )
  attach(Record)
  on.exit( detach(Record) )
  # Run model
  data( AI_pacific_ocean_perch, package="SpatialDeltaGLMM" )
  Data_Geostat = data.frame( "Catch_KG"=AI_pacific_ocean_perch[,'cpue..kg.km.2.'], "Year"=AI_pacific_ocean_perch[,'year'], "Vessel"="missing", "AreaSwept_km2"=1, "Lat"=AI_pacific_ocean_perch[,'start.latitude'], "Lon"=AI_pacific_ocean_perch[,'start.longitude'], "Pass"=0)
  Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
  Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=test_path )
  Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )
  TmbData = SpatialDeltaGLMM::Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method )
  TmbList = SpatialDeltaGLMM::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=test_path, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
  on.exit( dyn.unload(paste0(test_path,"/",TMB::dynlib(Record$Version))), add=TRUE )
  Opt = TMBhelper::Optimize( obj=TmbList[["Obj"]], getsd=FALSE, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]] )  # , rel.tol=1e-20
  # Comparisons
  expect_equal( Opt$par, opt$par, tolerance=1e-3 )
  expect_equal( Opt$objective, opt$objective, tolerance=1e-3 )
})

