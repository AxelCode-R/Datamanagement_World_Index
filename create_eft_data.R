source("global.R")

min_date <- as.Date("2010-01-01")

active_date <- "2022-05-19"
listed_etfs <- suppressMessages({
  httr::content(
    httr::GET(paste0("https://www.alphavantage.co/query?function=LISTING_STATUS&date=",active_date,"&apikey=",api_key,"&datatype=csv&state=active")), 
    as = "text", 
    encoding = "UTF-8"
  ) %>% 
    read_csv() %>% 
    filter(assetType == "ETF")
})


tickers <- listed_etfs %>% filter(ipoDate <= min_date) %>% pull(symbol) %>% unique()
length(tickers)
etf_data <- get_data_for_ticker_alphaadvantage(tickers = tickers, min_date = min_date, alpha_key = alpha_key)





