

#devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM")
library( mapdata )
LocalDir = "C:/Users/James.Thorson/Desktop/UW Hideaway/Website/FishViz/"

# Plot global coverage of database
png( file="C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/examples/global_coverage.png", width=8, height=4, res=600, units="in")
  map( "worldHires", mar=c(4,4,2,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(2,2,0,0), xlim=c(-180,180), xaxs="i", yaxs="i", interior=FALSE, resolution=0, fill=TRUE, col="grey", lwd=0.001, myborder=0 )
  box()
  axis(1)
  axis(2)
  mtext( side=1:2, outer=TRUE, text=c("Longitude", "Latitude"))
  title( "Global data coverage" )

  # Get extrapolation data
  for(i in 1:15 ){
    if(i==1) Extrapolation_List = SpatialDeltaGLMM::Prepare_WCGBTS_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas") )
    if(i==2) Extrapolation_List = SpatialDeltaGLMM::Prepare_BC_Coast_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas"), strata_to_use=c("SOG","WCVI","QCS","HS","WCHG") )
    if(i==3) Extrapolation_List = SpatialDeltaGLMM::Prepare_EBS_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas") )
    if(i==4) Extrapolation_List = SpatialDeltaGLMM::Prepare_GOA_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas") )
    if(i==5) Extrapolation_List = SpatialDeltaGLMM::Prepare_NWA_Extrapolation_Data_Fn( strata.limits=list('All_areas'=1:1e5) )
    if(i==6) Extrapolation_List = SpatialDeltaGLMM::Prepare_SA_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas"), region="west_coast" )
    if(i==7) Extrapolation_List = SpatialDeltaGLMM::Prepare_SA_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas"), region="south_coast" )
    if(i==8){
      data( iceland_cod, package="SpatialDeltaGLMM" )
      Data_Geostat = na.omit(data.frame( "Catch_KG"=iceland_cod[,'Catch_b'], "Year"=iceland_cod[,'year'], "Vessel"=1, "AreaSwept_km2"=iceland_cod[,'towlength'], "Lat"=iceland_cod[,'lat1'], "Lon"=iceland_cod[,'lon1']))
      Extrapolation_List = SpatialDeltaGLMM::Prepare_Other_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas"), observations_LL=Data_Geostat[,c('Lat','Lon')], maximum_distance_from_sample=15 )
    }
    if(i==9) Extrapolation_List = SpatialDeltaGLMM::Prepare_GSL_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas") )
    if(i==10){
      #load( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/data/chatham_rise_grid.rda" )
      Extrapolation_List = SpatialDeltaGLMM::Prepare_NZ_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas") )
    }
    if(i==11) Extrapolation_List = SpatialDeltaGLMM::Prepare_AI_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas") )
    if(i %in% 12:15 ){
      Survey = c("NS-IBTS", "BITS", "SWC-IBTS", "EVHOE")[i-11]
      Datras = download_datras( species_set=1, survey=Survey, years=1991:2015, quarters=switch(Survey,"NS-IBTS"=1,"BITS"=1,"SWC-IBTS"=1,"EVHOE"=4), localdir=LocalDir, verbose=FALSE )
      Extrapolation_List = SpatialDeltaGLMM::Prepare_Other_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas"), maximum_distance_from_sample=25, grid_dim_km=c(10,10), observations_LL=na.omit(cbind('Lon'=Datras$DF[,'ShootLong'],'Lat'=Datras$DF[,'ShootLat'])) )
    }          # FishData:::

    # Plot
    Which = which( Extrapolation_List$a_el>0 )     # 1:nrow(Extrapolation_List$a_el)
    set.seed(1)
    points( Extrapolation_List$Data_Extrap[Which,c('Lon','Lat')], pch=20, cex=0.01, col=rainbow(15)[i])
  }
dev.off()
