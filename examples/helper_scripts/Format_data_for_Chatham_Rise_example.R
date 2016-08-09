
library( ThorsonUtilities )
library( maps )
library( mapdata )

setwd( "C:/Users/James.Thorson/Desktop/Korea travel/Collaborations/2016 -- New Zealand example/chatham_rise_example_V2/" )

YearTable = read.table( "Year_Table.txt" )
Orig_Data = read.table( "CHAT.MD.species.CORE.station_catch.txt", header=TRUE)
#Orig_Data = cbind( Orig_Data, 'Year'=as.POSIXlt(Orig_Data[,'date_f'], format="%d-%m-%y")$year+1900 )
Orig_Data = cbind( Orig_Data, 'Year'=YearTable[match( as.character(Orig_Data[,'trip_code']), YearTable[,1]),2] )
table( Orig_Data$Year )

chatham_rise_hake = rename_columns( DF=Orig_Data[,c('HAK_kg_km2','lat_f','long_f','Year','eorw_f')], newname=c("Hake_kg_per_km2","Lat","Lon","Year","eorw_f"))
chatham_rise_hake[,c('Lat','Lon')] = trunc(chatham_rise_hake[,c('Lat','Lon')]/10000) + (trunc(chatham_rise_hake[,c('Lat','Lon')]/100)%%100)/60 +  (trunc(chatham_rise_hake[,c('Lat','Lon')]/1)%%100)/3600
chatham_rise_hake[,'Lat'] = -1 * chatham_rise_hake[,'Lat']
chatham_rise_hake[,'Lon'] = ifelse( chatham_rise_hake[,'eorw_f']=="W", 360-chatham_rise_hake[,'Lon'], chatham_rise_hake[,'Lon'])
chatham_rise_hake[,'Hake_kg_per_km2'] = sapply(chatham_rise_hake[,'Hake_kg_per_km2'], FUN=function(num){ifelse(is.null(num), NA, as.numeric(as.character(num)))} )
chatham_rise_hake[,'Hake_kg_per_km2'] = ifelse( is.na(chatham_rise_hake[,'Hake_kg_per_km2']), 0, chatham_rise_hake[,'Hake_kg_per_km2'] )
summary( chatham_rise_hake )

map( "worldHires" )
points( x=chatham_rise_hake$Lon, y=chatham_rise_hake$Lat )
summary( chatham_rise_hake )

setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM" )
devtools::use_data( chatham_rise_hake, pkg=getwd())

load( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/data/chatham_rise_hake.rda")
summary( chatham_rise_hake )
