options(scipen=999)
options(stringsAsFactors = FALSE)
options(max.print=100)



library(alphavantager)
library(dplyr)
library(readr)

api_key <- "W59FBR90IRDZ8LVX"

#https://www.nasdaq.com/market-activity/stocks/screener
ticker_data <- readr::read_csv("nasdaq_screener_1648817439518.csv") %>% 
  select(Symbol, Name, Sector, Industry)

daily_return_adjusted_data <- list()
overview_data <- list()
for(i in (min(length(daily_return_adjusted_data), length(overview_data))+1):nrow(ticker_data)){
  ticker <- ticker_data[i,]$Symbol
  
  print(paste0("ticker: ", ticker, "  iter: ",i, "  daily_return_adjusted_data:",length(daily_return_adjusted_data), "  overview_data:",length(overview_data)))
  
  start_time <- Sys.time()
  while(as.numeric(Sys.time()-start_time) <= 62){
    temp_overview <- data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=",ticker,"&apikey=",api_key)))
    
    if(ncol(temp_overview)>1){
      overview_data[[i]] <- temp_overview
      break
    }else if(ncol(temp_overview)==1 && !"Error.Message" %in% names(temp_overview)){
      cat("+")
      Sys.sleep(1)
    }else{
      print("Not found!")
      break
    }
  }
  
  start_time <- Sys.time()
  while(as.numeric(Sys.time()-start_time) <= 62){
    temp_daily_return <- httr::content(
      httr::GET(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=",ticker,"&outputsize=full&apikey=",api_key,"&datatype=csv")), 
      as = "text", 
      encoding = "UTF-8"
    )
    
    if(substr(temp_daily_return, 1, 9) == "timestamp"){
      daily_return_adjusted_data[[i]] <- cbind(
        "ticker"=ticker,
        temp_daily_return %>% 
          read_csv() %>% 
          arrange(timestamp)
      )
      break
    }else if(substr(temp_daily_return,8,11) == "Note" && !"Error.Message" %in% names(temp_daily_return)){
      cat("+")
      Sys.sleep(1)
    }else{
      print("Not found!")
      break
    }
  }

}


daily_return_adjusted_data <- bind_rows(daily_return_adjusted_data)
overview_data <- bind_rows(overview_data)
ticker_data <- ticker_data





daily_return_adjusted_data <- daily_return_adjusted_data %>% 
  arrange(ticker, timestamp) %>% 
  group_by(ticker) %>% 
  mutate(
    "return" = adjusted_close/dplyr::lag(adjusted_close, order_by = timestamp)-1
  ) %>% 
  filter(!is.na(return))
  

head(x)
save(daily_return_adjusted_data, overview_data, ticker_data, file = "world_index_data.rdata")



load("world_index_data.rdata")





# yahoo all stoxx symbols
# https://www.cboe.com/us/equities/market_statistics/listed_symbols/
# https://www.barchart.com/solutions/data/market/BXE


