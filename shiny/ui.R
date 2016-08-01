library(shiny)

# Load stuff
load( file="database/Results-speciesDF.RData" )
region_set = unique( speciesDF[,'Region'] )

# Page for user interface
fluidPage(

  titlePanel("Visualize fish populations"),

  # Panel for settings
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
      #actionButton("activate", "Click to plot or refresh selection"),
    br(),
    #br(),

    # Plot animated maps
    uiOutput("speciesMapSelex"),
    uiOutput("sliderSelex"),

    h1("Disclaimer"),
    h4("These results do not reflect all available information for assessing the status of these stocks, and anyone interested should consult stock assessments for a more-complete picture of indivudal stocks"),
    br()
  ),

  # Configuration of plotting tabs
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
  )
)
