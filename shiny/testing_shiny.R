

#Details of how to deploy the package at:
#http://shiny.rstudio.com/articles/shinyapps.html

#Details of other ways to host a Shiny App online:
#http://shiny.rstudio.com/tutorial/lesson7/

#Sign-in for hosting on shinyapps.io:
#http://www.shinyapps.io/

setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/shiny" )

##############
# Test locally
##############
library(shiny)
runApp()

##############
# Push to server
# Apparently, must ...
#  ... have all packages installed locally on the R version being used
##############
library(rsconnect)
deployApp()
