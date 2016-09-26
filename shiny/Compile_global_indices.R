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
#LocalDir = "C:/Users/James.Thorson/Desktop/UW Hideaway/Website/FishViz/"
##LocalDir = "C:/Users/Jim/Desktop/UW Hideaway/Website/FishViz/"
#ResultsDir = paste0(LocalDir,"database_nx=1000/")
##ResultsDir = paste0(LocalDir,"database_nx=500/")
#  dir.create(ResultsDir)
#
## Load libraries
#library(TMB)               # Can instead load library(TMBdebug)
#library(SpatialDeltaGLMM)
#
################
## Settings
################
#RegionSet = c("Eastern_Bering_Sea", "Gulf_of_Alaska", "Aleutian_Islands", "California_current", "NS-IBTS", "BITS", "SWC-IBTS", "EVHOE", "New_Zealand", "Gulf_of_St_Lawrence", "British_Columbia", "Northwest_Atlantic", "South_Africa")
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
#nspecies = 20
#
## Decide on case-specific settings for use when calculating indices
#strata.limits <- data.frame('STRATA'="All_areas")
#
################
## Run model for each species
################
#
###### Loop through regions
#for(rI in 6:8){
##for(rI in 1:length(RegionSet)){
#  # Which region
#  Region = RegionSet[rI]
#
#  # This is where all runs will be located
#  RegionDir = paste0(LocalDir,'Region=',Region,'\\')
#    dir.create(RegionDir)
#
#  # Regions with a public API  #   # FishData::
#    # scrape_data( region="California_current", ...) is identical to previous except 10,000 times smaller
#  if( Region %in% c("Eastern_Bering_Sea", "Gulf_of_Alaska", "Aleutian_Islands", "California_current")){
#    Database = scrape_data( region=Region, species_set=nspecies, error_tol=0.01, localdir=LocalDir )
#    Database = ThorsonUtilities::rename_columns( Database[,c('Sci','Wt','Year','Long','Lat')], newname=c('Sci','Catch_KG','Year','Lon','Lat') )
#    species_set = unique( Database[,'Sci'] )
#    Database = cbind( Database, 'AreaSwept_km2'=0.01 )
#    if( !("Vessel" %in% names(Database)) ) Database = cbind( Database, 'Vessel'=1 )
#    Database = na.omit( Database )
#  }
#  if( Region %in% c("NS-IBTS", "BITS", "SWC-IBTS", "EVHOE")){
#    Datras = FishData:::download_datras( species_set=nspecies, survey=Region, years=1991:2015, quarters=switch(Region,"NS-IBTS"=1,"BITS"=1,"SWC-IBTS"=1,"EVHOE"=4), localdir=LocalDir )
#    Database = ThorsonUtilities::rename_columns( Datras$DF[,c('species','expanded_weight','Year','ShootLong','ShootLat')], newname=c('Sci','Catch_KG','Year','Lon','Lat') )
#    Database = na.omit( Database )
#    species_set = unique( Database[,'Sci'] )
#    Database = cbind( Database, 'AreaSwept_km2'=0.01 )
#    if( !("Vessel" %in% names(Database)) ) Database = cbind( Database, 'Vessel'=1 )
#  }
#  if( Region %in% c("New_Zealand", "Gulf_of_St_Lawrence", "British_Columbia", "Northwest_Atlantic", "South_Africa")){
#    if( Region=="British_Columbia"){
#      data( BC_pacific_cod_example, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Catch_KG"=BC_pacific_cod_example[,'PCOD_WEIGHT'], "Year"=BC_pacific_cod_example[,'Year'], "Vessel"="missing", "AreaSwept_km2"=BC_pacific_cod_example[,'TOW.LENGTH..KM.']/100, "Lat"=BC_pacific_cod_example[,'LAT'], "Lon"=BC_pacific_cod_example[,'LON'], "Pass"=0)
#      species_set = "Gadus macrocephalus"
#    }
#    if( Region=="Gulf_of_St_Lawrence"){
#      data( GSL_american_plaice, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Year"=GSL_american_plaice[,'year'], "Lat"=GSL_american_plaice[,'latitude'], "Lon"=GSL_american_plaice[,'longitude'], "Vessel"="missing", "AreaSwept_km2"=GSL_american_plaice[,'swept'], "Catch_KG"=GSL_american_plaice[,'biomass']*GSL_american_plaice[,'vstd'] )
#      species_set = "Hippoglossoides platessoides"
#    }
#    if( Region=="Northwest_Atlantic"){
#      data( georges_bank_haddock_spring, package="SpatialDeltaGLMM" )         # standardized area swept = 0.0112 nm^2 = 0.0112*1.852^2 km^2
#      Database = data.frame( "Catch_KG"=georges_bank_haddock_spring[,'CATCH_WT_CAL'], "Year"=georges_bank_haddock_spring[,'YEAR'], "Vessel"="missing", "AreaSwept_km2"=0.0112*1.852^2, "Lat"=georges_bank_haddock_spring[,'LATITUDE'], "Lon"=georges_bank_haddock_spring[,'LONGITUDE'])
#      species_set = "Melanogrammus aeglefinus"
#    }
#    if( Region=="South_Africa"){
#      data( south_africa_westcoast_jacopever, package="SpatialDeltaGLMM" )         # standardized area swept = 0.0112 nm^2 = 0.0112*1.852^2 km^2
#      Database = data.frame( "Catch_KG"=south_africa_westcoast_jacopever[,'HELDAC'], "Year"=south_africa_westcoast_jacopever[,'Year'], "Vessel"="missing", "AreaSwept_km2"=south_africa_westcoast_jacopever[,'area_swept_nm2']*1.852^2, "Lat"=south_africa_westcoast_jacopever[,'cen_lat'], "Lon"=south_africa_westcoast_jacopever[,'cen_long'])
#      species_set = "Helicolenus dactylopterus"
#    }
#    if( Region=="Iceland"){
#      data( iceland_cod, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Catch_KG"=iceland_cod[,'Catch_b'], "Year"=iceland_cod[,'year'], "Vessel"=1, "AreaSwept_km2"=iceland_cod[,'towlength'], "Lat"=iceland_cod[,'lat1'], "Lon"=iceland_cod[,'lon1'])
#      species_set = "Gadus morhua"
#    }
#    if( Region=="New_Zealand"){
#      data( chatham_rise_hake, package="SpatialDeltaGLMM" )
#      Database = data.frame( "Catch_KG"=chatham_rise_hake[,'Hake_kg_per_km2'], "Year"=chatham_rise_hake[,'Year'], "Vessel"=1, "AreaSwept_km2"=1, "Lat"=chatham_rise_hake[,'Lat'], "Lon"=chatham_rise_hake[,'Lon'])
#      species_set = "Merluccius australis"
#    }
#    Database = na.omit( Database )
#  }
#
#  # Read or simulate trawl data
#  if( !('Sci' %in% names(Database)) & length(species_set)==1 ){
#    Database = cbind(Database, 'Sci'=species_set)
#  }
#
#  # Kick out species with all or no encounters in any year
#  EncProb = tapply( Database[,'Catch_KG'], INDEX=list(factor(Database[,'Sci'],levels=species_set),Database[,'Year']), FUN=function(vec){mean(vec>0)})
#  species_set = species_set[ which(apply(EncProb,MARGIN=1,FUN=function(vec){all(vec!=1&vec!=0)})) ]
#
#  # Kick out high-level taxa
#  species_set = species_set[ which(sapply(as.character(species_set),FUN=function(Char){length(strsplit(Char,split=" ")[[1]])==2})) ]
#
#  # Kick out "sp."
#  if( length(grep("sp.",species_set))>0 ) species_set = species_set[-grep(" sp.",species_set)]
#
#  ##### Loop through species
#  for( sI in 1:length(species_set)){
#
#    # Run model
#    if( !file.exists(paste0(ResultsDir,Region,"-",species_set[sI],".RData")) ){
#
#      # Subset data
#      Data_Geostat = Database[ which(Database[,'Sci']==species_set[sI]), ]
#
#      # Get extrapolation data
#      if( Region %in% c("British_Columbia","South_Africa","Iceland","Northwest_Atlantic","NS-IBTS","BITS","SWC-IBTS","EVHOE") ){
#        if( Region == "British_Columbia" ){
#          Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, strata_to_use=c("HS","QCS") )
#        }
#        if( Region == "South_Africa" ){
#          Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, region="west_coast" )
#        }
#        if( Region == "Iceland" ){
#          Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, observations_LL=Data_Geostat[,c('Lat','Lon')], maximum_distance_from_sample=15 )
#        }
#        if( Region == "Northwest_Atlantic" ){
#          Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=list('Georges_Bank'=c(1130,1140,1150,1160,1170,1180,1190,1200,1210,1220,1230,1240,1250,1290,1300)) )
#        }
#        if( Region %in% c("NS-IBTS", "BITS", "SWC-IBTS", "EVHOE")){
#          Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, observations_LL=Data_Geostat[,c('Lat','Lon')], maximum_distance_from_sample=25, grid_dim_km=c(10,10) )
#        }
#      }else{
#        Extrapolation_List = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, observations_LL=Data_Geostat[,c('Lat','Lon')] )
#      }
#
#      # Calculate spatial information for SPDE mesh, strata areas, and AR1 process
#      file.remove( paste0(RegionDir,"/","Kmeans-",n_x,".RData") )    # To avoid re-using the mesh, delete the file
#      Spatial_List = Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=RegionDir )
#      Data_Geostat = cbind( Data_Geostat, Spatial_List$loc_UTM, "knot_i"=Spatial_List$knot_i )
#
#      # Make TMB data list
#      TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )
#
#      # Make TMB object
#      TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=RegionDir, "Version"=Version, "RhoConfig"=RhoConfig, "VesselConfig"=VesselConfig, "loc_x"=Spatial_List$loc_x)
#      Obj = TmbList[["Obj"]]
#
#      # Run model
#      Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=NULL, bias.correct=FALSE )
#      Report = Obj$report()
#      Year_Set = min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])
#      dyn.unload(paste0(RegionDir,"/",TMB::dynlib(Version)))
#
#      # Plot index
#      Index = PlotIndex_Fn( DirName=RegionDir, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, strata_names=strata.limits[,1], use_biascorr=TRUE )
#
#      # Plot center of gravity
#      COG = Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Znames=colnames(TmbData$Z_xm), PlotDir=RegionDir)
#
#      # save
#      Save = list("Index"=Index$Table[Index$Table[,'Year']%in%Data_Geostat$Year,], "COG"=COG$COG_Table[COG$COG_Table[,'Year']%in%Data_Geostat$Year,], "EffectiveArea"=COG$EffectiveArea_Table[COG$EffectiveArea_Table[,'Year']%in%Data_Geostat$Year,])
#      # Add slots to rank species in importance for plotting
#      Save[["Summary"]] = c( "average_occurence"=mean(Data_Geostat[,'Catch_KG']>0), "average_density"=mean(Data_Geostat[,'Catch_KG']/Data_Geostat[,'AreaSwept_km2']) )
#      save( Save, file=paste0(ResultsDir,Region,"-",species_set[sI],".RData") )
#
#      # Plot maps for animation   # SpatialDeltaGLMM::
#      MapDetails_List = MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
#      Dir = paste0(ResultsDir,"Image-",Region,"-",species_set[sI],"/")
#      dir.create( Dir )
#      for(tI in 1:TmbData$n_t){
#        if( any((TmbData$t_i+1)==tI) ){
#          png( filename=paste0(Dir,Year_Set[tI],".png"), width=MapDetails_List$MapSizeRatio['Width(in)'], height=MapDetails_List$MapSizeRatio['Height(in)'], res=200, units="in" )
#            Zlim = range( log(Report$D_xt) )
#            # MappingDetails=MapDetails_List$MappingDetails; Mat=log(Report$D_xt[,tI,drop=FALSE]); zlim=Zlim; PlotDF=MapDetails_List[["PlotDF"]]; MapSizeRatio=MapDetails_List[["MapSizeRatio"]]; Xlim=MapDetails_List[["Xlim"]]; Ylim=MapDetails_List[["Ylim"]]; FileName=NA; Format=""; Year_Set=Year_Set[tI]; outermargintext=c("",""); mar=c(0,0,2,0); Cex=0.5
#            SpatialDeltaGLMM:::PlotMap_Fn(MappingDetails=MapDetails_List$MappingDetails, Mat=log(Report$D_xt[,tI,drop=FALSE]), zlim=Zlim, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=NA, Format="", Year_Set=Year_Set[tI], outermargintext=c("",""), mar=c(2,2,2,1), Cex=switch(Region, "Northwest_Atlantic"=2, "Gulf_of_St_Lawrence"=1, "NS-IBTS"=0.5, "BITS"=1.25, "SWC-IBTS"=1.2, "EVHOE"=0.65, 0.5), zone=MapDetails_List[["Zone"]], pch=15)
#            axis(1); axis(2)
#          dev.off()
#        }
#      }
#    }
#
#    # To be removed later
#    #if( file.exists(paste0(ResultsDir,Region,"-",species_set[sI],".RData")) ){
#    #  # Subset data
#    #  Data_Geostat = Database[ which(Database[,'Sci']==species_set[sI]), ]
#    #
#    #  # Add slots
#    #  load( file=paste0(ResultsDir,Region,"-",species_set[sI],".RData") )
#    #  if( !("Summary" %in% names(Save)) ){
#    #    Save[["Summary"]] = c( "average_occurence"=mean(Data_Geostat[,'Catch_KG']>0), "average_density"=mean(Data_Geostat[,'Catch_KG']/Data_Geostat[,'AreaSwept_km2']) )
#    #    save( Save, file=paste0(ResultsDir,Region,"-",species_set[sI],".RData") )
#    #  }
#    #}
#  }
#}
#
#
####################
## Compress into easy-to-load data frames
####################
#
## Which species are available
#file_set = list.files(ResultsDir)[setdiff(grep("RData",list.files(ResultsDir)),grep("Results",list.files(ResultsDir)))]
#
## Things to save
#indexDF = cogDF = NULL
#speciesDF = data.frame( matrix(NA, nrow=length(file_set), ncol=9, dimnames=list(NULL,c("Region","Sci","Common","Family","Order","Class","Phylum","average_occurence","average_density"))) )
#
## Loop through species
#for(sI in 1:nrow(speciesDF)){
#  # Get Genus-Species
#  Char = strsplit( file_set[sI], split="-", fixed=TRUE)[[1]]
#  speciesDF[sI,'Sci'] = strsplit( Char[2], split=".", fixed=TRUE)[[1]][1]
#  speciesDF[sI,'Region'] = Char[1]
#
#  # Load results
#  load( file=paste0(ResultsDir,file_set[sI]) )
#  indexDF <- rbind(indexDF, data.frame("Region"=speciesDF[sI,'Region'], "Species"=speciesDF[sI,'Sci'], Save$Index, "Index"=Save$Index[,'Estimate..metric.tonnes.']/mean(Save$Index[,'Estimate..metric.tonnes.'])) )
#  cogDF <- rbind(cogDF, data.frame("Region"=speciesDF[sI,'Region'], "Species"=speciesDF[sI,'Sci'], "Year"=Save$COG[which(Save$COG[,'m']==1),'Year'], "East"=Save$COG[which(Save$COG[,'m']==1),c('COG_hat','SE')], "North"=Save$COG[which(Save$COG[,'m']==2),c('COG_hat','SE')]) )
#
#  # Get common names for species with "genus[ ]species" names
#  if( " " %in% strsplit(as.character(speciesDF[sI,'Sci']),split="")[[1]] ){
#    NCBI = taxize::sci2comm(sciname=speciesDF[sI,'Sci'], db="ncbi", simplify=TRUE)
#    speciesDF[sI,'Common'] = ifelse(length(NCBI[[1]])==0,"-",NCBI[[1]])        # rev(ITIS[[1]])[1]
#  }else{
#    speciesDF[sI,'Common'] = "-"
#  }
#
#  # Add rankings
#  if( "Summary" %in% names(Save)){
#    speciesDF[sI,c("average_occurence","average_density")] = Save$Summary[c("average_occurence","average_density")]
#  }
#
#  # Get taxonomic info
#  # NCBI is weird, but doesn't fail
#  if(FALSE){
#    NCBI = taxize::classification(speciesDF[sI,'Sci'], db='ncbi')
#    if("rank"%in%names(NCBI[[1]]) && "superclass"%in%NCBI[[1]][,'rank']) speciesDF[sI,'Class'] = NCBI[[1]][which(NCBI[[1]][,'rank']=="superclass"),'name']
#    if("rank"%in%names(NCBI[[1]]) && "order"%in%NCBI[[1]][,'rank']) speciesDF[sI,'Order'] = NCBI[[1]][which(NCBI[[1]][,'rank']=="order"),'name']
#    if("rank"%in%names(NCBI[[1]]) && "family"%in%NCBI[[1]][,'rank']) speciesDF[sI,'Family'] = NCBI[[1]][which(NCBI[[1]][,'rank']=="family"),'name']
#  }
#  # Itis fails a lot
#  if(FALSE){
#    Itis = taxize::classification(speciesDF[sI,'Sci'], db='itis')
#    if("rank"%in%names(Itis[[1]]) && "Class"%in%Itis[[1]][,'rank']) speciesDF[sI,'Class'] = Itis[[1]][which(Itis[[1]][,'rank']=="Class"),'name']
#    if("rank"%in%names(Itis[[1]]) && "Order"%in%Itis[[1]][,'rank']) speciesDF[sI,'Order'] = Itis[[1]][which(Itis[[1]][,'rank']=="Order"),'name']
#    if("rank"%in%names(Itis[[1]]) && "Family"%in%Itis[[1]][,'rank']) speciesDF[sI,'Family'] = Itis[[1]][which(Itis[[1]][,'rank']=="Family"),'name']
#  }
#  if(TRUE){
#    COL = taxize::classification(speciesDF[sI,'Sci'], db='gbif', row=1)
#    if("rank"%in%names(COL[[1]]) && "phylum"%in%COL[[1]][,'rank']) speciesDF[sI,'Phylum'] = as.character(COL[[1]][which(COL[[1]][,'rank']=="phylum"),'name'])
#    if("rank"%in%names(COL[[1]]) && "class"%in%COL[[1]][,'rank']) speciesDF[sI,'Class'] = as.character(COL[[1]][which(COL[[1]][,'rank']=="class"),'name'])
#    if("rank"%in%names(COL[[1]]) && "order"%in%COL[[1]][,'rank']) speciesDF[sI,'Order'] = as.character(COL[[1]][which(COL[[1]][,'rank']=="order"),'name'])
#    if("rank"%in%names(COL[[1]]) && "family"%in%COL[[1]][,'rank']) speciesDF[sI,'Family'] = as.character(COL[[1]][which(COL[[1]][,'rank']=="family"),'name'])
#  }
#  #GBIF = taxize::classification(speciesDF[sI,'Sci'], db='gbif')
#  #EOL = taxize::classification(speciesDF[sI,'Sci'], db='eol')
#}
#
## Save compiled results
#save( speciesDF, file=paste0(ResultsDir,"Results-speciesDF.RData"))
#save( indexDF, file=paste0(ResultsDir,"Results-indexDF.RData"))
#save( cogDF, file=paste0(ResultsDir,"Results-cogDF.RData"))
#
#
