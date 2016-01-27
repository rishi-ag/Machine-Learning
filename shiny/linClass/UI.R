library(shiny)
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("The Playground!"),
      h4("Approved Parameters"),
      fluidRow(
        column(6, sliderInput("NoObs1", label = "# of Obs", min = 10, max = 200, value = 50)),
        column(6, sliderInput("Rho1", label = "Correlation features", min = -1, max = 1, value = -0.7, step = 0.1))
        ),
      fluidRow(
        column(3,numericInput("SD1X", label = "Wht SD", value = 10, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("SD1Y", label = "Ht SD", value = 20, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("MU1X", label = "Wht Mean", value = 20, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("MU1Y", label = "Ht Mean", value = 60, min = 0, max = 100, step = 5))
        ),
      h4("Denied Parameters"),
      fluidRow(
        column(6, sliderInput("NoObs2", label = "# of Obs", min = 10, max = 200, value = 50)),
        column(6, sliderInput("Rho2", label = "Correlation features", min = -1, max = 1, value = -0.1, step = 0.1))
      ),
      fluidRow(
        column(3,numericInput("SD2X", label = "Wht SD", value = 20, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("SD2Y", label = "Ht SD", value = 20, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("MU2X", label = "Wht Mean", value = 40, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("MU2Y", label = "Ht Mean", value = 80, min = 0, max = 100, step = 5))
      ),
      h4("Undecided Parameters"),
      fluidRow(
        column(6, sliderInput("NoObs3", label = "# of Obs", min = 10, max = 200, value = 50)),
        column(6, sliderInput("Rho3", label = "Correlation features", min = -1, max = 1, value = 0.5, step = 0.1))
      ),
      fluidRow(
        column(3,numericInput("SD3X", label = "Wht SD", value = 15, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("SD3Y", label = "Ht SD", value = 12, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("MU3X", label = "Wht Mean", value = 20, min = 0, max = 100, step = 5)) ,
        column(3,numericInput("MU3Y", label = "Ht Mean", value = 25, min = 0, max = 100, step = 5))
      )
      
    ),
    mainPanel(
      h2("Linear Classification"),
      p("This is an interactive app that lets you play with the classification of three types of data",
        em("You will find it incredibly fun"), "and at the same time get acquainted with some of the short comings of Linear Classification using a least squared discriminant."),
      p("Please note that the class sandwiched between 2 classes will usually have less than desirable performance.
        This is called the", span("masking effect", style = "color:blue")),
      p("To the side you may change the parameters of the three classes"),
      h3("Data and decision boundaries"),
      plotOutput("plot1",width = "600px", height = "400px"),
      h3("Errors"),
      tableOutput("table1")
    )
  )
))