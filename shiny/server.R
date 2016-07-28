library(shiny)

file_set = list.files("database")[grep("RData",list.files("database"))]
source( "Plot_Points_and_Bounds_Fn.R" )

DF1 = DF2 = NULL
species_s = region_s = rep(NA, length(file_set))
for(sI in 1:length(file_set)){
  Char = strsplit( file_set[sI], split="-", fixed=TRUE)[[1]]
  species_s[sI] = strsplit( Char[2], split=".", fixed=TRUE)[[1]][1]
  region_s[sI] = Char[1]
  load( file=paste0(getwd(),"/database/",file_set[sI]) )
  DF1 <- rbind(DF1, data.frame("Region"=region_s[sI], "Species"=species_s[sI], Save$Index, "Index"=Save$Index[,'Estimate..metric.tonnes.']/mean(Save$Index[,'Estimate..metric.tonnes.'])) )
  DF2 <- rbind(DF2, data.frame("Region"=region_s[sI], "Species"=species_s[sI], Save$COG[which(Save$COG[,'m']==2),]) )
}

# Settings
interval_width = 1

# Function containing things to display
function(input, output){

  # Plot abundance index
  output$plot1 <- renderPlot({
    input$activate
    isolate({
      plot( 1, type="n", xlim=range(DF1[,'Year']), ylim=range(DF1[,'Index']), xlab="Year", ylab="Relative abundance", main="Indices of population abundance")
      for( sI in 1:length(input$species)){
        Tmp = DF1[ which(DF1[,'Species']==input$species[sI]), ]
        Plot_Points_and_Bounds_Fn( y=Tmp[,'Index'], x=Tmp[,'Year'], ybounds=(Tmp[,'Index']%o%c(1,1))*exp(Tmp[,'SD..log.']%o%c(-interval_width,interval_width)), type="b", col="black", col_bounds=rgb(0,0,0,0.2), bounds_type="shading")
      }
    })
  })

  # Plot northward COG
  output$plot2 <- renderPlot({
    input$activate
    isolate({
      plot( 1, type="n", xlim=range(DF2[,'Year']), ylim=range(DF2[,'COG_hat']), xlab="Year", ylab="km north of equator", main="Northward center-of-gravity")
      for( sI in 1:length(input$species)){
        Tmp = DF2[ which(DF2[,'Species']==input$species[sI]), ]
        Plot_Points_and_Bounds_Fn( y=Tmp[,'COG_hat'], x=Tmp[,'Year'], ybounds=(Tmp[,'COG_hat']%o%c(1,1))+(Tmp[,'SE']%o%c(-interval_width,interval_width)), type="b", col="black", col_bounds=rgb(0,0,0,0.2), bounds_type="shading")
     }
    })
  })
}
