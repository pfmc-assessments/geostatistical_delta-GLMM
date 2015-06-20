PlotResultsOnMap_Fn <-
function(MappingDetails, Report, MapSizeRatio, Xlim, Ylim, FileName, Year_Set,
         Rotate=0, Format="png", Res=200, ...){
  D_it = Report$D_xt[NN_Extrap$nn.idx,]
  R1_it = Report$R1_xt[NN_Extrap$nn.idx,]
  R2_it = Report$R2_xt[NN_Extrap$nn.idx,]
  # adding Epsilon surfaces showing annual differences from shared distribution
  Eps1_it = Report$Epsilon1_st[NN_Extrap$nn.idx,]
  Eps2_it = Report$Epsilon2_st[NN_Extrap$nn.idx,]
  plot_codes <- c("Pres","Pos","Dens","Pos_Rescaled","Dens_Rescaled","Eps_Pres","Eps_Pos")
  for(RespI in 1:7){
    if(RespI==1){
      # Presence/absence ("Pres")
      Mat = R1_it 
    }
    if(RespI==2){
      # Positive values ("Pos")
      Mat = log(R2_it+quantile(R2_it,0.01))
      Mat = ifelse(Mat<(-5),-5,Mat)
    }
    if(RespI==3){
      # Density ("Dens")
      Mat = log(D_it+quantile(D_it,0.01))
      Mat = ifelse(Mat<(-5),-5,Mat)
    }
    if(RespI==4){
      # Positive values rescaled ("Pos_Rescaled")
      Mat = log(R2_it+quantile(R2_it,0.25))
    }
    if(RespI==5){
      # Density rescaled ("Dens_Rescaled")
      Mat = log(D_it+quantile(D_it,0.25))
    }
    if(RespI==6){
      # Epsilon for presence/absence ("Eps_Pres")
      Mat = Eps1_it # maybe should be exponentiated?
    }
    if(RespI==7){
      # Epsilon for positive values ("Eps_Pos")
      Mat = Eps2_it # maybe should be exponentiated?
    }
    # Do plot
    PlotMap_Fn( MappingDetails=MappingDetails, Mat=Mat, MapSizeRatio=MapSizeRatio, Xlim=Xlim, Ylim=Ylim, FileName=paste0(FileName,plot_codes[RespI]), Year_Set=Year_Set, Rotate=Rotate, Format=Format, Res=Res)
  }
}
