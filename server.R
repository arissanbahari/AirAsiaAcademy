#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
dfs = read.csv("C:/Users/User/Documents/AIRASIA ACADEMY R AND PYTHON/College.csv")
dfs

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Combine the selected variables into a new data frame
    selectedData <- reactive({ #reactive is join/react based on particular input. reactive makes new subtable
        dfs[, c(input$xcol, input$ycol)]# select all the row, select only xcol and ycol
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)#applying kmeans algorithm to teh selected table based on input clusters
    })
    
    output$plot1 <- renderPlot({#render plot to get the plot
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3) #by default rounfd shape
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4) #centroids
    })

})
