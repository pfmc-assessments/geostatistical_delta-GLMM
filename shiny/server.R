library(shiny)
library(ggplot2)
if( !"devtools" %in% installed.packages()[,1]) install.packages("devtools")
if( !"SpatialDeltaGLMM" %in% installed.packages()[,1]) devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM")

#savedir = "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/inst/extdata/"
savedir = system.file("extdata", package="SpatialDeltaGLMM")
file_set = list.files(savedir)[which(!file.info(paste0(savedir,"/",list.files(savedir)))$isdir)]

DF1 = DF2 = NULL
species_set = region_set = rep(NA, length(file_set))
for(sI in 1:length(file_set)){
  Char = strsplit( file_set[sI], split="-", fixed=TRUE)[[1]]
  species_set[sI] = strsplit( Char[2], split=".", fixed=TRUE)[[1]][1]
  region_set[sI] = Char[1]
  load( file=paste0(savedir,"/",file_set[sI]))
  DF1 <- rbind(DF1, data.frame("Region"=region_set[sI], "Species"=species_set[sI], Save$Index, "Index"=Save$Index[,'Estimate..metric.tonnes.']/mean(Save$Index[,'Estimate..metric.tonnes.'])) )
  DF2 <- rbind(DF2, data.frame("Region"=region_set[sI], "Species"=species_set[sI], Save$COG[which(Save$COG[,'m']==2),]) )
}

# Settings
interval_width = 1

function(input, output) {

  output$plot <- renderPlot({

    #if(input$test==TRUE) matplot( x=DF1[,'Year'], y=DF1[,'Estimate..metric.tonnes.'] )
    plot( 1, type="n", xlim=range(DF1[,'Year']), ylim=range(DF1[,'Index']))
    for( sI in 1:length(input$species)){
      Tmp = DF1[ which(DF1[,'Species']==input$species[sI]), ]
      #lines( x=Tmp[,'Year'], y=Tmp[,'Index'])
      SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Tmp[,'Index'], x=Tmp[,'Year'], ybounds=(Tmp[,'Index']%o%c(1,1))*exp(Tmp[,'SD..log.']%o%c(-interval_width,interval_width)), type="b", col="black", col_bounds=rgb(0,0,0,0.2), bounds_type="shading")
    }


  }, height=700)

}
