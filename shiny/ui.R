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
    # Choose region
    selectInput(inputId="region", label="Region to show", choices=region_set, multiple=FALSE, selected="Eastern_Bering_Sea"),

    # Based on region, select species
    checkboxGroupInput(inputId="species", label="Species to show", choices=species_s, selected=species_s),

    # Only update plot when clicked (to decrease server load)
    actionButton("activate", "Click to refresh plot after changing selection")
  ),

  mainPanel(
    plotOutput('plot1'),
    plotOutput('plot2')
  )
)
