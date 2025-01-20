library(shiny)
library(shinyjs)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  div(id = "app-container",
      titlePanel("Quantum Bootstrap Stock Simulator"),
      div(id = "controls",
          p("Explore the stock performance of leading quantum computing companies."),
          selectInput(
            "ticker",
            "Select Quantum Company:",
            choices = c(
              "IBM" = "IBM",
              "Google (Alphabet)" = "GOOGL",
              "Microsoft" = "MSFT",
              "Rigetti Computing" = "RGTI",
              "IonQ" = "IONQ",
              "Quantum Computing Inc." = "QUBT"
            ),
            width = "180px"
          ),
          actionButton("runSim", "Run Simulation", class = "btn-primary")
      ),
      tabsetPanel(
        tabPanel("Methods",
                 h3("Methods & Modeling Techniques"),
                 div(
                   h4("1. Quantum Bootstrapping"),
                   p("Quantum Bootstrapping applies randomization to resample the stock's historical closing prices."),
                   plotlyOutput("quantumPlot", height = "400px")
                 ),
                 div(
                   h4("2. Traditional Bootstrapping"),
                   p("Traditional Bootstrapping generates new samples by randomly resampling the original dataset."),
                   plotlyOutput("traditionalPlot", height = "400px")
                 ),
                 div(
                   h4("3. Linear Regression Prediction"),
                   p("A basic linear regression model to predict future prices."),
                   plotOutput("regressionPlotMethods", height = "400px"),
                   div(textOutput("rSquaredLinear"), style = "text-align: center; font-style: italic; color: #666;")
                 )
                 # Add other sections as needed
        ),
        tabPanel("Risk/Reward Calculator",
                 plotOutput("cumulativeReturnPlot", height = "400px"),
                 div(h4("Understanding Risk & Reward", style = "text-align: center; color: #999;")),
                 h3("Risk/Reward Metrics"),
                 tableOutput("riskMetrics")
        ),
        tabPanel("Data Summary",
                 div(id = "data-summary",
                     h3("Data Overview"),
                     verbatimTextOutput("dataSummary")
                 ),
                 plotOutput("stockLinePlot", height = "400px")
        )
      ),
      
      div(textOutput("debugText"), style = "font-size: 12px; color: red;"),
      
      tags$footer(
        "Made with ❤️ by HollyTech",
        style = "text-align: center; color: #6c757d; margin-top: 20px;"
      )
  )
)
