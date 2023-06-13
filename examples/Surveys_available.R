

#devtools::install_github("pfmc-assessments/geostatistical_delta-GLMM")
#devtools::install_github("james-thorson/FishData")
library( mapdata )
LocalDir = "C:/Users/James.Thorson/Desktop/UW Hideaway/Website/FishViz/"

# Colors for plots
set.seed(1)
Region_Colors = sample(rainbow(15))

# Plot global coverage of database
png( file="C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/examples/global_coverage.png", width=8, height=8, res=600, units="in")

  # Settings
  #par( mfrow=c(2,1), mar=c(2.5,2.5,1,0), tck=-0.02, mgp=c(1.75,0.25,0) )
  par( mfrow=c(2,1), mar=c(2,2,2,0), xaxs="i", yaxs="i", mgp=c(1.75,0.25,0), tck=-0.02, oma=c(2,2,0,0) )

  ##########################
  # Map of spatial coverage
  ##########################
  map( "worldHires", xlim=c(-180,180), mar=c(2,2,2,0), oma=c(0,0,0,0), xaxs="i", yaxs="i", interior=FALSE, resolution=0, fill=TRUE, col="grey", lwd=0.001, myborder=0 )
  box()                 # mar=c(4,4,2,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(0,0,0,0),
  axis(1)
  axis(2)
  mtext( side=1:2, outer=FALSE, text=c("Longitude", "Latitude"), line=1.5)
  title( "Spatial coverage" )

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
      Datras = FishData:::download_datras( species_set=1, survey=Survey, years=1991:2015, quarters=switch(Survey,"NS-IBTS"=1,"BITS"=1,"SWC-IBTS"=1,"EVHOE"=4), localdir=LocalDir, verbose=FALSE )
      Extrapolation_List = SpatialDeltaGLMM::Prepare_Other_Extrapolation_Data_Fn( strata.limits=data.frame('STRATA'="All_areas"), maximum_distance_from_sample=25, grid_dim_km=c(10,10), observations_LL=na.omit(cbind('Lon'=Datras$DF[,'ShootLong'],'Lat'=Datras$DF[,'ShootLat'])) )
    }          #

    # Plot
    Which = which( Extrapolation_List$a_el>0 )     # 1:nrow(Extrapolation_List$a_el)
    points( Extrapolation_List$Data_Extrap[Which,c('Lon','Lat')], pch=20, cex=0.01, col=Region_Colors[i])
  }

  ##########################
  # Plot of temperal coverage
  ##########################
  plot( 1, type="n", ylim=c(0,1000), xlim=c(1960,2016), xlab="", ylab="", main="Temporal coverage")
  mtext( side=c(1,2), text=c("Year","Samples per year"), outer=FALSE, line=1.5 )

  for(i in 1:15 ){
    if( i==1 ){
      data( WCGBTS_Canary_example, package="SpatialDeltaGLMM" )
      Data_Geostat = data.frame( "Catch_KG"=WCGBTS_Canary_example[,'HAUL_WT_KG'], "Year"=as.numeric(sapply(WCGBTS_Canary_example[,'PROJECT_CYCLE'],FUN=function(Char){strsplit(as.character(Char)," ")[[1]][2]})), "Vessel"=WCGBTS_Canary_example[,"VESSEL"], "AreaSwept_km2"=WCGBTS_Canary_example[,"AREA_SWEPT_HA"]/1e2, "Lat"=WCGBTS_Canary_example[,'BEST_LAT_DD'], "Lon"=WCGBTS_Canary_example[,'BEST_LON_DD'], "Pass"=WCGBTS_Canary_example[,'PASS']-1.5)
    }
    if( i==2 ){
      data( BC_pacific_cod_example, package="SpatialDeltaGLMM" )
      Data_Geostat = data.frame( "Catch_KG"=BC_pacific_cod_example[,'PCOD_WEIGHT'], "Year"=BC_pacific_cod_example[,'Year'], "Vessel"="missing", "AreaSwept_km2"=BC_pacific_cod_example[,'TOW.LENGTH..KM.']/100, "Lat"=BC_pacific_cod_example[,'LAT'], "Lon"=BC_pacific_cod_example[,'LON'], "Pass"=0)
    }
    if( i==3 ){
      data( EBS_pollock_data, package="SpatialDeltaGLMM" )
      Data_Geostat = data.frame( "Catch_KG"=EBS_pollock_data[,'catch'], "Year"=EBS_pollock_data[,'year'], "Vessel"="missing", "AreaSwept_km2"=0.01, "Lat"=EBS_pollock_data[,'lat'], "Lon"=EBS_pollock_data[,'long'], "Pass"=0)
    }
    if( i==4 ){
      data( GOA_pacific_cod , package="SpatialDeltaGLMM")
      Data_Geostat = data.frame( "Catch_KG"=GOA_pacific_cod[,'catch'], "Year"=GOA_pacific_cod[,'year'], "Vessel"="missing", "AreaSwept_km2"=0.01, "Lat"=GOA_pacific_cod[,'lat'], "Lon"=GOA_pacific_cod[,'lon'], "Pass"=0)
    }
    if( i==5 ){
      data( georges_bank_haddock_spring, package="SpatialDeltaGLMM" )         # standardized area swept = 0.0112 nm^2 = 0.0112*1.852^2 km^2
      Data_Geostat = data.frame( "Catch_KG"=georges_bank_haddock_spring[,'CATCH_WT_CAL'], "Year"=georges_bank_haddock_spring[,'YEAR'], "Vessel"="missing", "AreaSwept_km2"=0.0112*1.852^2, "Lat"=georges_bank_haddock_spring[,'LATITUDE'], "Lon"=georges_bank_haddock_spring[,'LONGITUDE'])
    }
    if( i==6 ){
      data( south_africa_westcoast_jacopever, package="SpatialDeltaGLMM" )         # standardized area swept = 0.0112 nm^2 = 0.0112*1.852^2 km^2
      Data_Geostat = data.frame( "Catch_KG"=south_africa_westcoast_jacopever[,'HELDAC'], "Year"=south_africa_westcoast_jacopever[,'Year'], "Vessel"="missing", "AreaSwept_km2"=south_africa_westcoast_jacopever[,'area_swept_nm2']*1.852^2, "Lat"=south_africa_westcoast_jacopever[,'cen_lat'], "Lon"=south_africa_westcoast_jacopever[,'cen_long'])
    }
    if( i==7 ) Data_Geostat = NULL
    if( i==8 ){
      data( iceland_cod, package="SpatialDeltaGLMM" )
      Data_Geostat = data.frame( "Catch_KG"=iceland_cod[,'Catch_b'], "Year"=iceland_cod[,'year'], "Vessel"=1, "AreaSwept_km2"=iceland_cod[,'towlength'], "Lat"=iceland_cod[,'lat1'], "Lon"=iceland_cod[,'lon1'])
    }
    if( i==9 ){
      data( GSL_american_plaice, package="SpatialDeltaGLMM" )
      Data_Geostat = data.frame( "Year"=GSL_american_plaice[,'year'], "Lat"=GSL_american_plaice[,'latitude'], "Lon"=GSL_american_plaice[,'longitude'], "Vessel"="missing", "AreaSwept_km2"=GSL_american_plaice[,'swept'], "Catch_KG"=GSL_american_plaice[,'biomass']*GSL_american_plaice[,'vstd'] )
    }
    if( i==10 ){
      data( chatham_rise_hake, package="SpatialDeltaGLMM" )
      Data_Geostat = data.frame( "Catch_KG"=chatham_rise_hake[,'Hake_kg_per_km2'], "Year"=chatham_rise_hake[,'Year'], "Vessel"=1, "AreaSwept_km2"=1, "Lat"=chatham_rise_hake[,'Lat'], "Lon"=chatham_rise_hake[,'Lon'])
    }
    if( i==11 ){
      data( AI_pacific_ocean_perch, package="SpatialDeltaGLMM" )
      Data_Geostat = data.frame( "Catch_KG"=AI_pacific_ocean_perch[,'cpue..kg.km.2.'], "Year"=AI_pacific_ocean_perch[,'year'], "Vessel"="missing", "AreaSwept_km2"=1, "Lat"=AI_pacific_ocean_perch[,'start.latitude'], "Lon"=AI_pacific_ocean_perch[,'start.longitude'], "Pass"=0)
    }
    if(i %in% 12:15 ){
      Survey = c("NS-IBTS", "BITS", "SWC-IBTS", "EVHOE")[i-11]
      Datras = FishData:::download_datras( species_set=1, survey=Survey, years=1991:2015, quarters=switch(Survey,"NS-IBTS"=1,"BITS"=1,"SWC-IBTS"=1,"EVHOE"=4), localdir=LocalDir, verbose=FALSE )    #
      Data_Geostat = ThorsonUtilities::rename_columns( Datras$DF[,c('ShootLat','ShootLong','Year','expanded_weight','species','HaulDur')], newname=c('Lat','Lon','Year','Catch_KG','Species','Duration_minutes') )
    }

    # Plot
    Table = table(Data_Geostat$Year)
    lines( x=as.numeric(names(Table)), y=Table, bg=Region_Colors[i], pch=21, cex=1)
    points( x=as.numeric(names(Table)), y=Table, bg=Region_Colors[i], pch=21, cex=1)
  }
dev.off()
