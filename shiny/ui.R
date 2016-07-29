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

    # Choose region
    selectInput(inputId="region", label="Region to show", choices=region_set, multiple=FALSE, selected="Eastern_Bering_Sea"),

    # Based on region, select species
    #checkboxGroupInput(inputId="species", label="Species to show", choices=species_s, selected=species_s),
    uiOutput("speciesSelex"),

    # Only update plot when clicked (to decrease server load)
    actionButton("activate", "Click to plot or refresh selection")
  ),

  mainPanel(
    #textOutput('debug_text'),
    plotOutput('plot1'),
    plotOutput('plot2')
  )
)
