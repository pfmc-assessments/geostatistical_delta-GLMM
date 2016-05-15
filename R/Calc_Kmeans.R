
#' Calculate location for knots approximating spatial variation
#'
#' \code{Calc_Kmeans} determines the location for a set of knots for approximating spatial variation
#'
#' @param n_x the number of knots to select
#' @param loc_orig a matrix with two columns where each row gives the 2-dimensional coordinates to be approximated
#' @param nstart the number of times that the k-means algorithm is run while searching for the best solution (default=100)
#' @param randomseed a random number seed
#' @param iter.max the number of iterations used per k-means algorithm (default=1000)
#' @param DirPath a directory where the algorithm looks for a previously-saved output (Default=NULL, which turns off this feature)
#' @param Save_Results a boolean stating whether to save the output (Default=TRUE)

#' @return Tagged list containing outputs
#' \describe{
#'   \item{centers}{a matrix with 2 columns and n_x rows}
#'   \item{cluster}{A vector with length \code{nrow(loc_orig)} specifying which row of \code{centers} corresponds to each row of loc_orig}
#'   \item{tot.withinss}{the objective function (to be minimized) for the selected k-means solution}
#' }

#' @export
Calc_Kmeans <-
function(n_x, loc_orig, nstart=100, randomseed=NULL, iter.max=1000, DirPath=NULL, Save_Results=TRUE){
  # get old seed
  oldseed = ceiling(runif(1,min=1,max=1e6))
  # fix new seed
  if( !is.null(randomseed) ) set.seed( round(randomseed) )
  old.options <- options()
  options( "warn" = -1 )
  on.exit( options(old.options) )
  if( is.null(DirPath) ) DirPath = paste0(getwd(),"/")
  # Only calculate if knots > number of locations
  if( length(unique(paste(loc_orig[,1],loc_orig[,2],sep="_")))<=n_x ){
    Unique = unique(paste(loc_orig[,1],loc_orig[,2],sep="_"))
    Kmeans = NULL
    Kmeans[["centers"]] = as.matrix(loc_orig[match(Unique,paste(loc_orig[,1],loc_orig[,2],sep="_")),])
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
  # fix to old seed
  if( !is.null(randomseed) ) set.seed( oldseed )
  # return stuff
  return( Kmeans )
}
