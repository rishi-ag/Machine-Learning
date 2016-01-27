if(!require("shiny")) install.packages("shiny")
if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("ggplot2")) install.packages("ggplot2")

source("helper.R")

shinyServer(
  function(input, output) {
    
      param.vector <- reactive({
          list(
          class1 = list(no = input$NoObs1, rho = input$Rho1, sd = c(input$SD1X, input$SD1Y), 
                        mean = c(input$MU1X, input$MU1Y), class.name = "Approved"),
          class2 = list(no = input$NoObs2, rho = input$Rho2, sd = c(input$SD2X, input$SD2Y), 
                        mean = c(input$MU2X, input$MU2Y), class.name = "Denied"),
          class3 = list(no = input$NoObs3, rho = input$Rho3, sd = c(input$SD3X, input$SD3Y),
                        mean =  c(input$MU3X, input$MU3Y), class.name = "Undecided")  
      )
          })
      
      data <- reactive({ GenData(param.vector()) })
      
      prediction <- reactive({ Classify(data()) })
      
      output$plot1 <- renderPlot({ PlotBoundary(prediction()[[1]], data()) })
      
      output$table1 <- renderTable({
          
          as.data.frame.matrix(table(data()$class, prediction()$prediction))
          
          })
  }
)