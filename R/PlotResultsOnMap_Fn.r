#' @export
PlotResultsOnMap_Fn <-
function(MappingDetails, Report, Nknots=Inf, PlotDF, MapSizeRatio, Xlim, Ylim, FileName, Year_Set=NULL, Years2Include=NULL, plot_set=1:5,
         Rescale=FALSE, Rotate=0, Format="png", Res=200, zone=NA, Cex=0.01, add=FALSE, category_names=NULL, textmargin=NULL, pch=NULL,
         Legend=list("use"=FALSE, "x"=c(10,30), "y"=c(10,30)), ...){

  # Fill in missing inputs
  if( "D_xt" %in% names(Report)){
    if( is.null(Year_Set) ) Year_Set = 1:ncol(Report$D_xt)
    if( is.null(Years2Include) ) Years2Include = 1:ncol(Report$D_xt)
    category_names = "singlespecies"
    Ncategories = length(category_names)
  }
  if( "D_xct" %in% names(Report)){
    if( is.null(Year_Set) ) Year_Set = 1:dim(Report$D_xct)[3]
    if( is.null(Years2Include) ) Years2Include = 1:dim(Report$D_xct)[3]
    if( is.null(Years2Include) ) category_names = 1:dim(Report$D_xct)[2]
    Ncategories = dim(Report$D_xct)[2]
  }
  if( "dhat_ktp" %in% names(Report)){
    if( is.null(Year_Set) ) Year_Set = 1:dim(Report$dhat_ktp)[2]
    if( is.null(Years2Include) ) Years2Include = 1:dim(Report$dhat_ktp)[2]
    if( is.null(Years2Include) ) category_names = 1:dim(Report$dhat_ktp)[3]
    Ncategories = dim(Report$dhat_ktp)[3]
  }

  # Extract elements
  plot_codes <- c("Pres", "Pos", "Dens", "Pos_Rescaled", "Dens_Rescaled", "Eps_Pres", "Eps_Pos", "LinPred_Pres", "LinPred_Pos")
  if( is.null(textmargin)){
    textmargin <- c("Probability of encounter", "Density, ln(kg. per square km.)", "Density, ln(kg. per square km.)", "", "", "", "", "", "")
  }

  # Select locations to plot
  if( Nknots<Inf ){
    NN_plot = RANN::kmeans(x=PlotDF[,c("Lon","Lat")], centers=Nknots, iter.max=50, nstart=2, trace=0)
    Match = match( 1:Nknots, NN_plot$cluster)
    PlotDF = PlotDF[Match,]
    message( "Restricted plotting locations to ", Nknots, " locations" )
  }

  # Loop through plots
  for( cI in 1:Ncategories){
    for(plot_num in plot_set){
      if(plot_num==1){
        # Presence/absence ("Pres")
        if("D_xt"%in%names(Report)) Mat = Report$R1_xt
        if("D_xct"%in%names(Report)) Mat = Report$R1_xct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==2){
        # Positive values ("Pos")
        if("D_xt"%in%names(Report)) Mat = log(Report$R2_xt)
        if("D_xct"%in%names(Report)) Mat = log(Report$R2_xct)[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==3){
        # Density ("Dens")
        if("D_xt"%in%names(Report)) Mat = log(Report$D_xt)
        if("D_xct"%in%names(Report)) Mat = log(Report$D_xct)[,cI,]
        if("dhat_ktp"%in%names(Report)) Mat = Report$dhat_ktp[,,cI]
      }
      if(plot_num==4){
        # Positive values rescaled ("Pos_Rescaled")
        if("D_xt"%in%names(Report)) Mat = log(Report$R2_xt+quantile(Report$R2_xt,0.25))
        if("D_xct"%in%names(Report)) Mat = log(Report$R2_xct+quantile(Report$R2_xct,0.25))[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==5){
        # Density rescaled ("Dens_Rescaled")
        if("D_xt"%in%names(Report)) Mat = log(Report$D_xt+quantile(Report$D_xt,0.25))
        if("D_xct"%in%names(Report)) Mat = log(Report$D_xct+quantile(Report$D_xct,0.25))[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==6){
        # Epsilon for presence/absence ("Eps_Pres")
        if("D_xt"%in%names(Report)) Mat = Report$Epsilon1_st
        if("D_xct"%in%names(Report)) Mat = Report$Epsilon1_sct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==7){
        # Epsilon for positive values ("Eps_Pos")
        if("D_xt"%in%names(Report)) Mat = Report$Epsilon2_st
        if("D_xct"%in%names(Report)) Mat = Report$Epsilon2_sct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==8){
        # Linear predictor for probability of encounter
        if("D_xt"%in%names(Report)) Mat = Report$P1_xt
        if("D_xct"%in%names(Report)) Mat = Report$P1_xct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      if(plot_num==9){
        # Linear predictor for positive catch rates
        if("D_xt"%in%names(Report)) Mat = Report$P2_xt
        if("D_xct"%in%names(Report)) Mat = Report$P2_xct[,cI,]
        if("dhat_ktp"%in%names(Report)) stop("Not implemented for SpatialVAM")
      }
      # Do plot    #
      Return = SpatialDeltaGLMM:::PlotMap_Fn( MappingDetails=MappingDetails, Mat=Mat[,Years2Include], PlotDF=PlotDF, MapSizeRatio=MapSizeRatio, Xlim=Xlim, Ylim=Ylim, FileName=paste0(FileName,plot_codes[plot_num],ifelse(Ncategories>1,paste0("--",category_names[cI]),"")), Year_Set=Year_Set[Years2Include], Rescale=Rescale, Rotate=Rotate, Format=Format, Res=Res, zone=zone, Cex=Cex, textmargin=textmargin[plot_num], add=add, pch=pch, Legend=Legend, ...)
    }          #
  }
  return( invisible(Return) )
}
