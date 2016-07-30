library(shiny)
library(ggplot2)

file_set = list.files("database")[grep("RData",list.files("database"))]

DF1 = DF2 = NULL
species_s = region_s = rep(NA, length(file_set))
for(sI in 1:length(file_set)){
  Char = strsplit( file_set[sI], split="-", fixed=TRUE)[[1]]
  species_s[sI] = strsplit( Char[2], split=".", fixed=TRUE)[[1]][1]
  region_s[sI] = Char[1]
}
region_set = unique(region_s)

fluidPage(

  titlePanel("Visualize fish populations"),

  sidebarPanel(
    # Display useful info
    h1("Background"),
    h4("This page shows indices of abundance and distribution for 10 species from several regions"),
    h4("For details of computation, please see ", a("www.FishStats.org", href="http://www.FishStats.org")),
    br(),

    # Time series plots
    h1("Time series plot settings"),
    # Choose region
    selectInput(inputId="region", label="Region to show", choices=region_set, multiple=FALSE, selected="Eastern_Bering_Sea"),
    # Based on region, select species
    uiOutput("speciesSelex"),
    # Only update plot when clicked (to decrease server load)
    actionButton("activate", "Click to plot or refresh selection"),
    br(),
    br(),

    # Plot animated maps
    h1("Maps settings"),
    selectInput(inputId="species2animate", label="Species to animate", choices=paste(region_s,"-",species_s,sep=""), selected="Eastern_Bering_Sea-Gadus chalcogrammus", multiple=FALSE),
    sliderInput(inputId="sliderYear", label="Year of animation", min=1982, max=2015, value=1982, step=1, animate=animationOptions(interval=500, loop=FALSE, playButton="PLAY", pauseButton=NULL))
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Time series", plotOutput('plot1'), plotOutput('plot2')),
      tabPanel("Maps", plotOutput('image1'))
    )
    #textOutput('debug_text'),
    #plotOutput('plot1'),
    #plotOutput('plot2'),
    #plotOutput('image1')       #
  )
)
