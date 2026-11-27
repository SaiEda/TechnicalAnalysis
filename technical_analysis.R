library(quantmod)

load_stock_data <- function(file_name = "portfolio.txt") {
  
  symbols <- readLines(file_name)
  
  stock_data <- list()
  
  for (symbol in symbols) {
    
    getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    ) -> data
    
    stock_data[[symbol]] <- data
  }
  
  return(stock_data)
}
stocks <- load_stock_data()

print(stocks)

calculate_statistics <- function(stock_data) {
  
  closing_price <- as.numeric(Cl(stock_data))
  
  moving_average <- SMA(closing_price, n = 20)
  
  mean_value <- mean(
    closing_price,
    na.rm = TRUE
  )
  
  mode_value <- as.numeric(
    names(
      sort(
        table(closing_price),
        decreasing = TRUE
      )
    )[1]
  )
  
  median_value <- median(
    closing_price,
    na.rm = TRUE
  )
  
  standard_deviation <- sd(
    closing_price,
    na.rm = TRUE
  )
  
  statistics <- data.frame(
    Mean = mean_value,
    Mode = mode_value,
    Median = median_value,
    Standard_Deviation = standard_deviation
  )
  
  return(list(
    Statistics = statistics,
    Moving_Average = moving_average
  ))
}
stock_statistics <- lapply(
  stocks,
  calculate_statistics
)
stock_statistics$AAPL$Statistics
stock_statistics$MSFT$Statistics
display_stock_data <- function(stock_data) {
  
  print(head(stock_data, 10))
  
  print(tail(stock_data, 10))
  
  print(summary(stock_data))
}
display_stock_data(stocks$AAPL)
chart_stock <- function(stock_data) {
  
  chartSeries(
    stock_data,
    type = "line",
    theme = chartTheme("white"),
    name = "Stock Price"
  )
}
chart_stock(stocks$AAPL)
chartSeries(
  stocks$AAPL,
  TA = "addSMA(n = 20)",
  theme = chartTheme("white"),
  name = "AAPL with 20-Day Moving Average"
)