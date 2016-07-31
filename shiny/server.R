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
  DF2 <- rbind(DF2, data.frame("Region"=region_s[sI], "Species"=species_s[sI], "Year"=Save$COG[which(Save$COG[,'m']==1),'Year'], "East"=Save$COG[which(Save$COG[,'m']==1),c('COG_hat','SE')], "North"=Save$COG[which(Save$COG[,'m']==2),c('COG_hat','SE')]) )
}

# Settings
interval_width = 1

# Function containing things to display
function(input, output){

  #### Dynamic user inputs
  # The following reactive function would return the column variable names corresponding to the dataset selected by the user.
  species_subset <- reactive({
    species_s[region_s==input$region]
  })
  # Select species
  output$speciesSelex <- renderUI({
    #selectInput("variablex", "Select the First (X) variable", choices = var())
    checkboxGroupInput(inputId="species", label="Species to show", choices=species_subset(), selected=species_subset())
  })

  # Select species for mapping animation
  output$speciesMapSelex <- renderUI({
    selectInput(inputId="species2animate", label="Species to map", choices=species_subset(), selected=species_subset()[1], multiple=FALSE)
  })

  # Select years for mapping animation
  Num_years <- reactive({
    dir <- paste0("database/Image-",input$region,"-",input$species2animate,"/")
    length( list.files(dir) )
  })
  output$sliderSelex <- renderUI({
    sliderInput(inputId="sliderNum", label="Year of animation", min=1, max=Num_years(), value=1, step=1, animate=animationOptions(interval=500, loop=FALSE, playButton="PLAY", pauseButton="PAUSE"))
  })

  #### Plots
  # Plot abundance index
  output$plot1 <- renderPlot({
    input$activate
    isolate({
      par( xaxs="i" )
      plot( 1, type="n", xlim=range(DF1[which(DF1[,'Region']==input$region),'Year']), ylim=c(0,max(DF1[which(DF1[,'Region']==input$region),'Index'])*1.2), xlab="Year", ylab="Relative abundance", main="Indices of population abundance")
      for( sI in 1:length(input$species)){
        Tmp = DF1[ which(DF1[,'Species']==input$species[sI] & DF1[,'Region']==input$region), ]
        Plot_Points_and_Bounds_Fn( y=Tmp[,'Index'], x=Tmp[,'Year'], ybounds=(Tmp[,'Index']%o%c(1,1))*exp(Tmp[,'SD..log.']%o%c(-interval_width,interval_width)), type="b", col=rainbow(length(input$species))[sI], col_bounds=rainbow(length(input$species),alpha=0.2)[sI], bounds_type="shading")
      }
      if(length(input$species)>0) legend( "top", legend=input$species, fill=rainbow(length(input$species)), bty="n", ncol=5 )
    })
  })

  # Plot northward COG
  output$plot2 <- renderPlot({
    input$activate
    isolate({
      par( xaxs="i" )
      plot( 1, type="n", xlim=range(DF2[which(DF2[,'Region']==input$region),'Year']), ylim=range(DF2[which(DF2[,'Region']==input$region),'North.COG_hat']%o%c(1,1)+(DF2[which(DF2[,'Region']==input$region),'North.SE']%o%c(-interval_width,interval_width))), xlab="Year", ylab="kilometers north of equator", main="Northward center-of-gravity")
      for( sI in 1:length(input$species)){
        Tmp = DF2[ which(DF2[,'Species']==input$species[sI] & DF1[,'Region']==input$region), ]
        Plot_Points_and_Bounds_Fn( y=Tmp[,'North.COG_hat'], x=Tmp[,'Year'], ybounds=(Tmp[,'North.COG_hat']%o%c(1,1))+(Tmp[,'North.SE']%o%c(-interval_width,interval_width)), type="b", col=rainbow(length(input$species))[sI], col_bounds=rainbow(length(input$species),alpha=0.2)[sI], bounds_type="shading")
     }
    })
  })

  # Plot northward COG
  output$plot3 <- renderPlot({
    input$activate
    isolate({
      par( xaxs="i" )
      plot( 1, type="n", xlim=range(DF2[which(DF2[,'Region']==input$region),'Year']), ylim=range(DF2[which(DF2[,'Region']==input$region),'East.COG_hat']%o%c(1,1)+(DF2[which(DF2[,'Region']==input$region),'East.SE']%o%c(-interval_width,interval_width))), xlab="Year", ylab="kilometers east of regional reference", main="Eastward center-of-gravity")
      for( sI in 1:length(input$species)){
        Tmp = DF2[ which(DF2[,'Species']==input$species[sI] & DF1[,'Region']==input$region), ]
        Plot_Points_and_Bounds_Fn( y=Tmp[,'East.COG_hat'], x=Tmp[,'Year'], ybounds=(Tmp[,'East.COG_hat']%o%c(1,1))+(Tmp[,'East.SE']%o%c(-interval_width,interval_width)), type="b", col=rainbow(length(input$species))[sI], col_bounds=rainbow(length(input$species),alpha=0.2)[sI], bounds_type="shading")
     }
    })
  })

  # Disply species
  output$debug_text <- renderPrint({
    dir <- paste0("database/Image-",input$region,"-",input$species2animate,"/")
    paste0(dir,list.files(dir)[input$sliderSelex])
  })

  # Plot animation images
  output$image1 <- renderImage({
    dir <- paste0("database/Image-",input$region,"-",input$species2animate,"/")
    list(src=paste0(dir,list.files(dir)[input$sliderNum]), contentType="image/png", alt="This is alternate text") # , width=600, height="auto"
  }, deleteFile=FALSE)
}
