#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
dfs = read.csv("C:/Users/User/Documents/AIRASIA ACADEMY R AND PYTHON/College.csv")
dfs

vars = setdiff(names(dfs), "Private")
vars
# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
    headerPanel('College'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', vars),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 5)
    ),
    mainPanel(
        plotOutput('plot1')
    )
))
