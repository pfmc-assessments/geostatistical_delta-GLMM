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
    h1("Plot settings"),
    # Choose region
    selectInput(inputId="region", label="Region to show", choices=region_set, multiple=FALSE, selected="Eastern_Bering_Sea"),
    # Based on region, select species
    uiOutput("speciesSelex"),
    # Only update plot when clicked (to decrease server load)
    actionButton("activate", "Click to plot or refresh selection"),
    br(),
    br(),

    # Plot animated maps
    uiOutput("speciesMapSelex"),
    uiOutput("sliderSelex")
  ),

  mainPanel(
    tabsetPanel(
      # Time series tab
      tabPanel("Time series: Index",
        #textOutput('debug_text'),
        plotOutput('plot1', height="600px")    #
      ),
      tabPanel("Time series: Distribution",
        plotOutput('plot2'),
        plotOutput('plot3')
      ),
      # Mapping tab
      tabPanel("Maps",
        plotOutput('image1')
      )
    )
    #plotOutput('plot1'),
    #plotOutput('plot2'),
    #plotOutput('image1')       #
  )
)
