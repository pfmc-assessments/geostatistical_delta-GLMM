
# Tutorial: http://r-pkgs.had.co.nz/tests.html
# And example see: https://github.com/ss3sim/ss3sim/tree/master/tests/testthat
context("Testing examples")

# Aleutian Islands
# Test default + Random-walk on spatio-temporal variation
test_that("Aleutian Islands groundfish bottom trawl survey, POP is working ", {
  skip_on_travis()
  # Prepping
  test_path = file.path(example_path,"AI_POP") # Aparrently must run in test_path, and not expample_path
  load( file.path(test_path,"opt.RData") )
  load( file.path(test_path,"Record.RData") )
  attach(Record)
  on.exit( detach(Record) )
  # Run model
  data( AI_pacific_ocean_perch, package="SpatialDeltaGLMM" )
  Data_Geostat = data.frame( "Catch_KG"=AI_pacific_ocean_perch[,'cpue..kg.km.2.'], "Year"=AI_pacific_ocean_perch[,'year'], "Vessel"="missing", "AreaSwept_km2"=1, "Lat"=AI_pacific_ocean_perch[,'start.latitude'], "Lon"=AI_pacific_ocean_perch[,'start.longitude'], "Pass"=0)
  Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits )
  Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=test_path )
  Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )
  TmbData = SpatialDeltaGLMM::Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method )
  TmbList = SpatialDeltaGLMM::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=test_path, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
  on.exit( dyn.unload(paste0(test_path,"/",TMB::dynlib(Record$Version))), add=TRUE )
  Opt = TMBhelper::Optimize( obj=TmbList[["Obj"]], getsd=FALSE, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]] )  # , rel.tol=1e-20
  # Comparisons
  expect_equal( Opt$par, opt$par, tolerance=1e-3 )
  expect_equal( Opt$objective, opt$objective, tolerance=1e-3 )
})
