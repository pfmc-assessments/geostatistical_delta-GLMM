

#Details of how to deploy the package at:
#http://shiny.rstudio.com/articles/shinyapps.html

#Details of other ways to host a Shiny App online:
#http://shiny.rstudio.com/tutorial/lesson7/

#Sign-in for hosting on shinyapps.io:
#http://www.shinyapps.io/

#Shiny cheat-sheet
#https://www.rstudio.com/wp-content/uploads/2016/01/shiny-cheatsheet.pdf

# Shiny examples
#https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# REAL VERSION -- DO NOT USE FOR TESTING
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/shiny" )

# DEVELOPMENT VERSION -- USE FOR TESTING
setwd( "C:/Users/James.Thorson/Desktop/Project_git/geostatistical_delta-GLMM/shiny/testing" )

##############
# Test locally
##############
library(shiny)
runApp()

##############
# Push to server
# Apparently, must ...
#  ... have all packages installed locally on the R version being used
#  .. currently working with Revolution Open R v3.2.2
##############
library(rsconnect)
deployApp()
