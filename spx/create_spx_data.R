library(lubridate)
options(scipen=999)
options(stringsAsFactors = FALSE)
options(max.print=100)



library(alphavantager)
library(dplyr)
library(readr)

# Load dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest)
wikispx <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
currentconstituents <- wikispx %>%
  html_node("#constituents") %>%
  html_table(header = TRUE)
currentconstituents

spxchanges <- wikispx %>%
  html_node("#changes") %>%
  html_table(header = FALSE, fill = TRUE) %>%
  filter(row_number() > 2) %>% # First two rows are headers
  `colnames<-`(c('Date','AddTicker','AddName','RemovedTicker','RemovedName','Reason')) %>%
  mutate(Date = as.Date(Date, format = "%B %d, %Y"),
         year = year(Date),
         month = month(Date))
spxchanges




# Start at the current constituents…
currentmonth <- as.Date(format(Sys.Date(), '%Y-%m-01'))
monthseq <- seq.Date(as.Date('1990-01-01'), currentmonth, by = 'month') %>% rev()

spxstocks <- currentconstituents %>% mutate(Date = currentmonth) %>% select(Date, Ticker = Symbol, Name = Security)
lastrunstocks <- spxstocks

# Iterate through months, working backwards
for (i in 2:length(monthseq)) {
  d <- monthseq[i]
  y <- year(d)
  m <- month(d)
  changes <- spxchanges %>%
    filter(year == year(d), month == month(d))
  
  # Remove added tickers (we’re working backwards in time, remember)
  tickerstokeep <- lastrunstocks %>%
    anti_join(changes, by = c("Ticker" = "AddTicker")) %>%
    mutate(Date = d)
  
  # Add back the removed tickers…
  tickerstoadd <- changes %>%
    filter(!RemovedTicker == "") %>%
    transmute(Date = d,
              Ticker = RemovedTicker,
              Name = RemovedName)
  
  thismonth <- tickerstokeep %>% bind_rows(tickerstoadd)
  spxstocks <- spxstocks %>% bind_rows(thismonth)
  
  lastrunstocks <- thismonth
}

uni_spx_ticker <- spxstocks$Ticker %>% unique()


api_key <- "W59FBR90IRDZ8LVX"

#https://www.nasdaq.com/market-activity/stocks/screener
tickers <- uni_spx_ticker

spx_daily_return_adjusted_data <- list()
spx_overview_data <- list()
for(i in 1:length(tickers)){
  ticker <- tickers[i]
  
  print(paste0("ticker: ", ticker, "  iter: ",i, "  spx_daily_return_adjusted_data:",length(spx_daily_return_adjusted_data), "  spx_overview_data:",length(spx_overview_data)))
  
  start_time <- Sys.time()
  while(as.numeric(Sys.time()-start_time) <= 62){
    temp_overview <- data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=",ticker,"&apikey=",api_key)))
    
    if(ncol(temp_overview)>1){
      spx_overview_data[[i]] <- temp_overview
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
      spx_daily_return_adjusted_data[[i]] <- cbind(
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


spx_daily_return_adjusted_data <- bind_rows(spx_daily_return_adjusted_data)
spx_overview_data <- bind_rows(spx_overview_data)


spx_daily_return_adjusted_data <- spx_daily_return_adjusted_data %>% 
  arrange(ticker, timestamp) %>% 
  group_by(ticker) %>% 
  mutate(
    "return_adjust_c" = adjusted_close/dplyr::lag(adjusted_close, order_by = timestamp)-1,
    "return_open" = open/dplyr::lag(open, order_by = timestamp)-1,
    "return_high" = high/dplyr::lag(high, order_by = timestamp)-1,
    "return_low" = low/dplyr::lag(low, order_by = timestamp)-1,
    "return_close" = close/dplyr::lag(close, order_by = timestamp)-1
  ) %>% 
  filter(!is.na(return))

#save(spx_daily_return_adjusted_data, spx_overview_data, file="spx_index_data.rdata")


spx_returns_replicated <- spx_daily_return_adjusted_data
uni_dates <- sort(as.Date(c(Sys.Date()+1, unique(spxstocks$Date))), decreasing = F)
for(i in 1:(length(uni_dates)-1)){
  start <- uni_dates[i]
  end <- uni_dates[i+1]
  tickers <- spxstocks %>% 
    filter(Date==start) %>% 
    pull(Ticker) %>% 
    unique()
  spx_returns_replicated <- spx_returns_replicated %>% 
    filter(timestamp < start | timestamp >= end | (timestamp >= start & timestamp < end & ticker %in% tickers))
}


spx_bm <- spx_returns_replicated %>% 
  group_by(timestamp) %>% 
  summarise(
    return_adjust_c = sum(return_adjust_c*volume, na.rm = T)/sum(volume, na.rm = T)
  ) %>% 
  ungroup()

spx_bm <- xts(spx_bm$return_adjust_c, order.by=spx_bm$timestamp)
names(spx_bm) <- "SPX"


# for(src in list.files("C:\\Users\\Axel\\Desktop\\Master-Thesis-All\\Index_Tracking_Optimization\\R\\")){
#   source(paste0("C:\\Users\\Axel\\Desktop\\Master-Thesis-All\\Index_Tracking_Optimization\\R\\", src))
# }
plotly_line_chart_xts(spx_bm)
plotly_line_chart_xts(return_to_cumret(spx_bm))


#save(spx_daily_return_adjusted_data, spx_overview_data, spx_bm, spxstocks, file="spx_index_data.rdata")

#https://fred.stlouisfed.org/series/SP500
spx_bm <- read.csv("SP500.csv") %>% 
  filter(SP500 != ".") %>% 
  mutate(SP500 = as.numeric(SP500)) %>% 
  mutate(return = SP500/lag(SP500, order_by = DATE)-1) %>% 
  filter(!is.na(return))
spx_bm <- xts(spx_bm$return, order.by=as.Date(spx_bm$DATE))
names(spx_bm) <- "SPX"
plotly_line_chart_xts(return_to_cumret(spx_bm))

save(spx_daily_return_adjusted_data, spx_overview_data, spx_bm, spxstocks, file="spx_index_data.rdata")

