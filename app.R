library(ggplot2)
library(dplyr)
library(quantmod)
library(plotly)
library(shinyjs)
library(glmnet)
library(PerformanceAnalytics)


# UI
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
      # Tab Panels
      tabsetPanel(
        # Methods Tab
        tabPanel("Methods",
                 h3("Methods & Modeling Techniques"),
                 
                 # Explanation Section
                 div(
                   h4("1. Quantum Bootstrapping"),
                   p("Quantum Bootstrapping applies randomization to resample the stock's historical closing prices, offering a distribution of potential future prices."),
                   plotlyOutput("quantumPlot", height = "400px")
                 ),
                 div(
                   h4("2. Traditional Bootstrapping"),
                   p("Traditional Bootstrapping generates new samples by randomly resampling the original dataset with replacement, simulating potential future price movements."),
                   plotlyOutput("traditionalPlot", height = "400px")
                 ),
                 div(
                   h4("3. Linear Regression Prediction"),
                   p("A basic linear regression model to predict future prices based on historical data."),
                   plotOutput("regressionPlotMethods", height = "400px"),
                 ),
                 div(
                   h4("4. Ridge Regression"),
                   p("A regression technique that applies a penalty to reduce overfitting."),
                   sliderInput("ridgeAlpha", "Ridge Alpha:", min = 0.01, max = 10, value = 1, step = 0.01),
                   plotOutput("ridgePlot", height = "400px"),
                 ),
                 
                 # Lasso Regression Section
                 div(
                   h4("5. Lasso Regression"),
                   p("A regression technique that applies a penalty to shrink some coefficients to zero, useful for feature selection."),
                   sliderInput("lassoAlpha", "Lasso Alpha:", min = 0.01, max = 10, value = 1, step = 0.01),
                   plotOutput("lassoPlot", height = "400px"),
                 ),
                 div(
                   h4("6. Quantum Regression"),
                   p("A regression model that uses Quantum Bootstrapped means to predict future prices."),
                   plotOutput("quantumRegressionPlot", height = "400px"),
        ),
        tabPanel("Risk/Reward Calculator",
                 plotOutput("cumulativeReturnPlot", height = "400px"),
                 div(h4("Understanding Risk & Reward", style = "text-align: center; color: #999;")),
                 h3("Risk/Reward Metrics"),
                 tableOutput("riskMetrics"),
                 ),
        ),
        tabPanel("Data Summary",
                 div(id = "data-summary",
                     h3("Data Overview"),
                     verbatimTextOutput("dataSummary")
                 ),
                 div(id = "summary-plot-title", "Closing Prices for AAPL"),
                 plotOutput("stockLinePlot", height = "400px"),
                 div(h4("Methods & Models (Coming Soon)", style = "text-align: center; color: #999;")),
                 ),
        
        tabPanel("Quantum Randomness",
               h3("Quantum Randomness as a Predictor"),
               p("Exploring if quantum randomness can improve stock price predictions."),
               plotOutput("quantumRandomPlot", height = "400px"),
               p("The R² value indicates how well the model fits the data. Higher values mean better predictive power.")
        ),
      ),
      
      div(textOutput("debugText"), style = "font-size: 12px; color: red;"),
      div(style = "text-align: center; font-style: italic; color: #666;",
          textOutput("rSquaredText")
      ),
      
      tags$footer(
        "Made with ❤️ by HollyTech",
        style = "text-align: center; color: #6c757d; margin-top: 20px;"
      )
  )
)



# Server
server <- function(input, output) {
  
  observeEvent(input$runSim, {
    # Fetch real stock data
    stock_data <- tryCatch({
      getSymbols(input$ticker, src = "yahoo", auto.assign = FALSE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(stock_data)) {
      showNotification("Error: Could not fetch data for the selected ticker.", type = "error")
      return()
    }
    
    stock_close <- Cl(stock_data)
    
    # Quantum Bootstrapping
    quantum_means <- replicate(1000, mean(sample(stock_close, replace = TRUE)))
    
    # Traditional Bootstrapping
    traditional_means <- replicate(1000, mean(sample(stock_close, size = length(stock_close), replace = TRUE)))
    
    # Quantum Bootstrapping Plot
    output$quantumPlot <- renderPlotly({
      plot_ly(
        x = ~quantum_means,
        type = "histogram",
        marker = list(
          color = "#FFD700",  # Orange color
          line = list(color = "black", width = 1)  # Black borders
        )
      ) %>%
        layout(
          title = list(text = "Quantum Bootstrapped Means", font = list(size = 24)),
          xaxis = list(title = "Mean Price", tickangle = -45),
          yaxis = list(title = "Frequency"),
          paper_bgcolor = "#F9F9FB",
          plot_bgcolor = "#FFFFFF",
          margin = list(l = 50, r = 50, t = 60, b = 60)  # Left, Right, Top, Bottom margins
        )
    })
    
    # Traditional Bootstrapping Plot
    output$traditionalPlot <- renderPlotly({
      plot_ly(
        x = ~traditional_means,
        type = "histogram",
        marker = list(
          color = "#003366",
          line = list(color = "black", width = 1)  # Black borders
        )
      ) %>%
        layout(
          title = list(text = "Traditional Bootstrapped Means", font = list(size = 24)),
          xaxis = list(title = "Mean Price", tickangle = -45),
          yaxis = list(title = "Frequency"),
          paper_bgcolor = "#F9F9FB",
          plot_bgcolor = "#FFFFFF",
          margin = list(l = 50, r = 50, t = 60, b = 60)  # Left, Right, Top, Bottom margins
        )
    })
    
    # Reactive function to fetch stock data
    stock_prices <- reactive({
      req(input$ticker)  # Make sure the ticker is selected
      stock_data <- getSymbols(input$ticker, src = "yahoo", auto.assign = FALSE)
      Cl(stock_data)  # Return closing prices
    })
    
    rSquaredValues <- reactiveValues(
      linear = NA,
      ridge = NA,
      lasso = NA,
      quantum = NA
    )
    
    
    # Reactive Data Summary
    output$dataSummary <- renderPrint({
      stock_close <- stock_prices()  # Get the reactive stock data
      summary_data <- summary(stock_close)
      cat("Data Summary for", input$ticker, "\n\n")
      
      cat("\nDate Range:\n")
      cat(index(stock_close)[1], "to", index(stock_close)[length(stock_close)])
      
      cat("\nNumber of Observations:\n")
      cat(length(stock_close))
    })
    
    # Line Plot of Stock Prices
    output$stockLinePlot <- renderPlot({
      stock_close <- stock_prices()
      df <- data.frame(Date = index(stock_close), Price = as.numeric(stock_close))
      
      ggplot(df, aes(x = Date, y = Price)) +
        geom_line(color = "#003366", size = 1) +
        theme_minimal() +
        labs(title = paste("Closing Prices for", input$ticker),
             x = "Date", y = "Price ($)")
    })
    
    output$rSquaredText <- renderText({
      paste("Linear R²:", round(rSquaredValues$linear, 3),
            "| Ridge R²:", round(rSquaredValues$ridge, 3),
            "| Lasso R²:", round(rSquaredValues$lasso, 3),
            "| Quantum R²:", round(rSquaredValues$quantum, 3))
    })
    
    # Perform a linear regression on closing prices
    output$regressionPlot <- renderPlot({
      stock_close <- stock_prices()
      df <- data.frame(Date = index(stock_close), Price = as.numeric(stock_close))
      
      # Fit Linear Model
      lm_model <- lm(Price ~ Date, data = df)
      
      # Calculate R²
      rSquaredLinear <- summary(lm_model)$r.squared
      rSquaredValues$linear <- rSquaredLinear
      
      # Plot
      ggplot(df, aes(x = Date, y = Price)) +
        geom_line(color = "#003366", size = 1) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#FF5733") +
        theme_minimal() +
        labs(title = "Linear Regression Model",
             subtitle = paste("R² =", round(r_squared, 3)),
             x = "Date", y = "Price ($)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    # Linear Regression Plot for the Methods Tab
    output$regressionPlotMethods <- renderPlot({
      lm_model <- lm(stock_close ~ index(stock_data))
      predicted_prices <- predict(lm_model)
      
      ggplot() +
        geom_line(aes(x = index(stock_data), y = stock_close), color = "blue") +
        geom_line(aes(x = index(stock_data), y = predicted_prices), color = "red", linetype = "dashed") +
        theme_minimal() +
        labs(title = "Linear Regression Model", x = "Date", y = "Price ($)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    # Ridge Regression Plot
    output$ridgePlot <- renderPlot({
      stock_close <- stock_prices()
      days_since_start <- as.numeric(index(stock_close) - min(index(stock_close)))  # Convert Date to numeric
      
      # Prepare the data with polynomial features
      X <- as.matrix(cbind(
        days_since_start,
        days_since_start^2,
        days_since_start^3
      ))
      Y <- as.numeric(stock_close)
      
      # Fit Ridge Regression
      ridge_model <- glmnet(X, Y, alpha = 0, lambda = input$ridgeAlpha)
      predicted_ridge <- predict(ridge_model, newx = X)
      # Calculate R²
      ridge_preds <- predicted_ridge
      ss_total <- sum((Y - mean(Y))^2)
      ss_residual <- sum((Y - ridge_preds)^2)
      rSquaredRidge <- 1 - (ss_residual / ss_total)
      rSquaredValues$ridge <- rSquaredRidge
      
      # Plot
      ggplot() +
        geom_line(aes(x = days_since_start, y = Y), color = "blue", size = 1) +
        geom_line(aes(x = days_since_start, y = predicted_ridge), color = "red", linetype = "dashed", size = 1) +
        theme_minimal() +
        labs(title = "Ridge Regression", x = "Days Since Start", y = "Closing Price ($)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    # Lasso Regression Plot
    output$lassoPlot <- renderPlot({
      stock_close <- stock_prices()
      days_since_start <- as.numeric(index(stock_close) - min(index(stock_close)))  # Convert Date to numeric
      
      # Prepare the data with polynomial features
      X <- as.matrix(cbind(
        days_since_start,
        days_since_start^2,
        days_since_start^3
      ))
      Y <- as.numeric(stock_close)
      
      # Fit Lasso Regression
      lasso_model <- glmnet(X, Y, alpha = 1, lambda = input$lassoAlpha)
      predicted_lasso <- predict(lasso_model, newx = X)
      
      lasso_preds <- predicted_lasso
      ss_total <- sum((Y - mean(Y))^2)
      ss_residual <- sum((Y - lasso_preds)^2)
      rSquaredLasso <- 1 - (ss_residual / ss_total)
      rSquaredValues$lasso <- rSquaredLasso
      
      # Plot
      ggplot() +
        geom_line(aes(x = days_since_start, y = Y), color = "blue", size = 1) +
        geom_line(aes(x = days_since_start, y = predicted_lasso), color = "green", linetype = "dashed", size = 1) +
        theme_minimal() +
        labs(title = "Lasso Regression", x = "Days Since Start", y = "Closing Price ($)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })

    # Risk/Reward Metrics Calculation
    output$riskMetrics <- renderTable({
      returns <- dailyReturn(stock_close)
      
      # Metrics Calculation
      sharpe_ratio <- round(mean(returns) / sd(returns) * sqrt(252), 3)
      sortino_ratio <- round(mean(returns) / sd(returns[returns < 0]) * sqrt(252), 3)
      max_drawdown <- round(maxDrawdown(returns), 3)
      calmar_ratio <- round(mean(returns) * 252 / abs(max_drawdown), 3)
      volatility <- round(sd(returns) * sqrt(252), 3)
      
      # Beta Calculation (Relative to S&P 500)
      sp500_data <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)
      sp500_returns <- dailyReturn(Cl(sp500_data))
      
      # Beta (Market Sensitivity)
      common_dates <- index(returns) %in% index(sp500_returns)
      aligned_stock <- returns[common_dates]
      aligned_sp500 <- sp500_returns[common_dates]
      
      beta <- round(cov(aligned_stock, aligned_sp500) / var(aligned_sp500), 3)
      
      
      # Create a data frame of the metrics
      data.frame(
        Metric = c(
          "Sharpe Ratio (Risk-adjusted return. Higher is better.)",
          "Sortino Ratio (Focuses on downside risk. Higher is better.)",
          "Max Drawdown (Largest peak-to-trough drop. Lower is better.)",
          "Calmar Ratio (Return vs. Max Drawdown. Higher is better.)",
          "Volatility (Annualized standard deviation. Measures risk.)",
          "Beta (Sensitivity to market movements. <1 = safer, >1 = riskier.)"
        ),
        Value = c(sharpe_ratio, sortino_ratio, max_drawdown, calmar_ratio, volatility, beta)
      )
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    output$dataSummary <- renderPrint({
      stock_close <- stock_prices()  # Get the reactive stock data
      summary_data <- summary(stock_close)
      
      # Print summary data
      cat("Data Summary for", input$ticker, "\n\n")
      
      # Print date range
      cat("\nDate Range:\n")
      cat(index(stock_close)[1], "to", index(stock_close)[length(stock_close)])
      
      # Print number of observations
      cat("\nNumber of Observations:\n")
      cat(length(stock_close))
    })
    
    output$cumulativeReturnPlot <- renderPlot({
      returns <- dailyReturn(stock_close)  # Calculate daily returns
      cumulative_returns <- cumprod(1 + returns) - 1  # Calculate cumulative returns
      
      # Get S&P 500 data for comparison
      sp500_data <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)
      sp500_returns <- dailyReturn(Cl(sp500_data))
      cumulative_sp500 <- cumprod(1 + sp500_returns) - 1
      
      # Align dates between your stock and S&P 500
      common_dates <- index(cumulative_returns)[index(cumulative_returns) %in% index(cumulative_sp500)]
      df <- data.frame(
        Date = common_dates,
        Stock_Returns = as.numeric(cumulative_returns[common_dates]),
        SP500_Returns = as.numeric(cumulative_sp500[common_dates])
      )
      
      # Plot cumulative returns
      ggplot(df, aes(x = Date)) +
        geom_line(aes(y = Stock_Returns, color = "Your Stock"), size = 1.2) +
        geom_line(aes(y = SP500_Returns, color = "S&P 500"), size = 1.2, linetype = "dashed") +
        scale_color_manual(values = c("Your Stock" = "green", "S&P 500" = "blue")) +
        theme_minimal() +
        labs(title = "Cumulative Returns", x = "Date", y = "Cumulative Return (%)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    # Sample matching number of dates
    sampled_dates <- sample(index(stock_data), length(quantum_means))
    
    # Create a data frame for the quantum regression model
    quantum_df <- data.frame(Date = sampled_dates, MeanPrice = quantum_means)
    
    # Quantum Regression Model
    output$quantumRegressionPlot <- renderPlot({
      quantum_model <- lm(MeanPrice ~ Date, data = quantum_df)
      predicted_prices <- predict(quantum_model, newdata = quantum_df)
      rSquaredQuantum <- summary(quantum_model)$r.squared
      ggplot(quantum_df, aes(x = Date, y = MeanPrice)) +
        geom_line(color = "lightblue") +
        geom_line(aes(y = predicted_prices), color = "green", linetype = "dashed") +
        theme_minimal() +
        labs(title = "Quantum Regression Model", x = "Date", y = "Mean Price ($)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
  })
}

# Run the App
shinyApp(ui, server)



