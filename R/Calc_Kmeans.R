Calc_Kmeans <-
function(n_x, Kmeans_Config, Data_Geostat, Data_Extrap){
  old.options <- options()
  options( "warn" = -1 ) 
  on.exit( options(old.options) )   
  if( paste0("Kmeans-",n_x,".RData") %in% list.files(getwd()) ){
    load( file=paste(DateFile,"Kmeans.RData",sep=""))
  }else{
    Kmeans = list( "tot.withinss"=Inf )
    for(i in 1:Kmeans_Config[["nstart"]]){       
      if(Kmeans_Config[["Locs"]]=="Samples"){
        Tmp = kmeans( x=Data_Geostat[,c('E_km','N_km')], centers=n_x, iter.max=Kmeans_Config[["iter.max"]], nstart=1, trace=0)
      }
      if(Kmeans_Config[["Locs"]]=="Domain"){
        Tmp = kmeans( x=Data_Extrap[,c('E_km','N_km')], centers=n_x, iter.max=Kmeans_Config[["iter.max"]], nstart=1, trace=0) # K$tot.withinss
      }
      print( paste0('Num=',i,' Current_Best=',round(Kmeans$tot.withinss,1),' New=',round(Tmp$tot.withinss,1)) )#,' Time=',round(Time,4)) )
      if( Tmp$tot.withinss < Kmeans$tot.withinss ){
        Kmeans = Tmp
      }
    }
  }
  return( Kmeans )
}
