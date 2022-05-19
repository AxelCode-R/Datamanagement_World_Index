source("global.R")

tickers <- c("A", "AAPL", "IBM")
min_date <- as.Date("2010-01-01")
daily_prices = TRUE
daily_return = TRUE
overview = TRUE
NAV = TRUE



active_date <- Sys.Date()
listed_etfs <- suppressMessages({
  httr::content(
    httr::GET(paste0("https://www.alphavantage.co/query?function=LISTING_STATUS&date=",active_date,"&apikey=",api_key,"&datatype=csv&state=active")), 
    as = "text", 
    encoding = "UTF-8"
  ) %>% 
    read_csv() %>% 
    filter(assetType == "ETF")
})


etf_data <- get_data_for_ticker_alphaadvantage(tickers = unique(listed_etfs$symbol), min_date = min_date, alpha_key = alpha_key)
