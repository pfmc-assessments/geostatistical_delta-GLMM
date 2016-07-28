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

fluidPage(

  titlePanel("Visualize fish populations"),

  sidebarPanel(

    #checkboxInput("test", "test"),
    #checkboxInput(species_set[1], species_set[1]),

    #selectInput(inputId="species", label="Species to show", choices=species_set, multiple=TRUE, selected=species_set)
    checkboxGroupInput(inputId="species", label="Species to show", choices=species_set, selected=species_set)
  ),

  mainPanel(
    plotOutput('plot')
  )
)
