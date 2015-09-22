Calc_Kmeans <-
function(n_x, loc_orig, nstart=100, randomseed=NULL, iter.max=1000, DirPath=NULL, Save_Results=TRUE){
  if( !is.null(randomseed) ) set.seed( round(randomseed) )
  old.options <- options()
  options( "warn" = -1 )
  on.exit( options(old.options) )
  if( is.null(DirPath) ) DirPath = paste0(getwd(),"/")
  if( length(unique(paste(loc_orig[,1],loc_orig[,2],sep="_")))<=n_x ){
    Unique = unique(paste(loc_orig[,1],loc_orig[,2],sep="_"))
    Kmeans = NULL
    Kmeans[["centers"]] = loc_orig[match(Unique,paste(loc_orig[,1],loc_orig[,2],sep="_")),]
    Kmeans[["cluster"]] = match( paste(loc_orig[,1],loc_orig[,2],sep="_"), Unique )
    message( "n_x less than n_unique so no calculation necessary" )
  }else{
    if( paste0("Kmeans-",n_x,".RData") %in% list.files(DirPath) ){
      load( file=paste0(DirPath,"/","Kmeans-",n_x,".RData") )
      message( "Loaded from ",DirPath,"/","Kmeans-",n_x,".RData" )
    }else{
      Kmeans = list( "tot.withinss"=Inf )
      for(i in 1:nstart){
        Tmp = kmeans( x=loc_orig, centers=n_x, iter.max=iter.max, nstart=1, trace=0)
        message( 'Num=',i,' Current_Best=',round(Kmeans$tot.withinss,1),' New=',round(Tmp$tot.withinss,1) )#,' Time=',round(Time,4)) )
        if( Tmp$tot.withinss < Kmeans$tot.withinss ){
          Kmeans = Tmp
        }
      }
      if(Save_Results==TRUE) save( Kmeans, file=paste0(DirPath,"/","Kmeans-",n_x,".RData"))
      message( "Calculated and saved to ",DirPath,"/","Kmeans-",n_x,".RData" )
    }
  }
  return( Kmeans )
}
