options(scipen=999)
options(stringsAsFactors = FALSE)
options(max.print=100)



library(alphavantager)
library(dplyr)
library(readr)
library(lubridate)

api_key <- "W59FBR90IRDZ8LVX"


# create monthly listing

monthly_listed_stocks <- list()
months <- seq.Date(from=as.Date("2010-01-01"), to=Sys.Date(), by="months")

for(i in 1:(length(months))){
  print(paste0("i: ",i))
  
  start_time <- Sys.time()
  while(as.numeric(Sys.time()-start_time) <= 62){
    monthly_listed_stocks[[i]] <- suppressMessages(try({
      httr::content(
        httr::GET(paste0("https://www.alphavantage.co/query?function=LISTING_STATUS&date=",months[i],"&apikey=",api_key,"&datatype=csv&state=active")), 
        as = "text", 
        encoding = "UTF-8"
      ) %>% 
        read_csv() %>% 
        filter(assetType == "Stock") %>% 
        mutate(month = months[i])
    }))
    if(nrow(monthly_listed_stocks[[i]])>1){
      break
    }else{
      Sys.sleep(3)
    }
  }
  
}

monthly_listed_stocks <- bind_rows(monthly_listed_stocks)

# test
monthly_listed_stocks %>% group_by(month) %>% summarise(n=n()) %>% View()
sum(unique(monthly_listed_stocks$month) %in% months)/length(unique(monthly_listed_stocks$month))

monthly_listed_stocks <- monthly_listed_stocks %>% filter(month >= ipoDate-years(1))
#save(monthly_listed_stocks, file="monthly_listed_stocks.rdata")


ticker_all <- unique(monthly_listed_stocks$symbol)
daily_return_adjusted_data <- list()

for(i in 1:length(ticker_all)){
  ticker <- ticker_all[i]
  print(paste0("i: ",i))
  
  start_time <- Sys.time()
  suppressMessages(while(as.numeric(Sys.time()-start_time) <= 62){
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
          filter(timestamp>=as.Date("2010-01-01")) %>% 
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
  })
  
}


daily_return_adjusted_data <- bind_rows(daily_return_adjusted_data)


daily_return_adjusted_data <- daily_return_adjusted_data %>% 
  arrange(ticker, timestamp) %>% 
  group_by(ticker) %>% 
  mutate(
    "return_adj_close" = adjusted_close/dplyr::lag(adjusted_close, order_by = timestamp)-1
  ) %>% 
  filter(!is.na(return))

#save(daily_return_adjusted_data, file = "daily_return_adjusted_data2.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/daily_return_adjusted_data.rdata")
daily_return_adjusted_data <- bind_rows(daily_return_adjusted_data)
daily_return_adjusted_data <- daily_return_adjusted_data %>% select( ticker, timestamp, adjusted_close)

daily_return_adjusted_data <- daily_return_adjusted_data %>% 
  arrange(ticker, timestamp) %>% 
  group_by(ticker) %>% 
  mutate(
    "return_adj_close" = adjusted_close/dplyr::lag(adjusted_close, order_by = timestamp)-1
  ) %>% 
  filter(!is.na(return))


save(daily_return_adjusted_data, file="daily_return_adjusted_data_final.rdata")




library(xts)
library(dplyr)
library(tidyr)



daily_return_adjusted_data <- daily_return_adjusted_data %>% 
  pivot_wider(names_from = "ticker", values_from = "return_adj_close") %>%
  arrange(timestamp) %>%
  data.frame()



daily_return_adjusted_data <- xts(daily_return_adjusted_data %>% select(-c("timestamp")), order.by=as.Date(daily_return_adjusted_data$timestamp))




# 
load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/daily_return_adjusted_data_final.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/monthly_listed_stocks.rdata")


monthly_listed_stocks_agg <- monthly_listed_stocks %>%
  group_by(symbol) %>%
  summarise(
    max_date = max(month),
    min_date = min(month),
    n=n(),
    l=length(unique(month))
  ) %>%
  filter(min_date < as.Date("2022-04-01"))

save(monthly_listed_stocks_agg, monthly_listed_stocks, file="monthly_listed_stocks.rdata")
# 
# daily_return_adjusted_data <- daily_return_adjusted_data %>% filter(ticker %in% unique(monthly_listed_stocks$symbol))
# return_adj_close_list <- list()
# 
# tickers <- unique(daily_return_adjusted_data$ticker)
# tickers <- split(tickers, ceiling(seq_along(tickers)/100))
# for(i in 1:length(tickers)){
#   #if((i %% 100) == 0){print(paste0(i," of ", length(tickers)))}
#   print(paste0(i," of ", length(tickers)))
#   
#   temp <- daily_return_adjusted_data %>% 
#     filter(ticker %in% tickers[[i]]) %>% 
#     pivot_wider(names_from = "ticker", values_from = "return_adj_close") %>%
#     arrange(timestamp) %>%
#     data.frame()
#   return_adj_close_list[[i]] <- xts(temp %>% select(-c("timestamp")), order.by = temp$timestamp)
#   
# }
# 

ticker_1 <- unique(daily_return_adjusted_data$ticker)
ticker_2 <- ticker_1[1:round(length(ticker_1)/2)]
ticker_1 <- ticker_1[!ticker_1 %in% ticker_2]

x = spread(daily_return_adjusted_data %>% filter(ticker %in% ticker_1[1:100]), key="ticker", value="return_adj_close")






# test
library(xts)
library(dplyr)
library(tidyr)


load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/monthly_listed_stocks.rdata")
load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/daily_return_adjusted_data_final.rdata")

start_date <- as.Date("2014-01-01")

tickers <- monthly_listed_stocks_agg %>% 
  filter(min_date <= start_date) %>% 
  pull(symbol) %>% 
  unique()

daily_return_adjusted_data <- daily_return_adjusted_data %>% 
  filter(
    timestamp >= start_date,
    ticker %in% tickers
  )

ticker_with_full_history <- daily_return_adjusted_data %>% group_by(ticker) %>% summarise(n=n()) %>% filter(n==max(n)) %>% pull(ticker)
daily_return_adjusted_data_with_fully_history <- daily_return_adjusted_data %>% filter(ticker %in% ticker_with_full_history)
rm(daily_return_adjusted_data)
# save(ticker_with_full_history, daily_return_adjusted_data_with_fully_history, file="world_index_data_v2.rdata")
# 
# daily_return_adjusted_data_with_fully_history_xts <- daily_return_adjusted_data_with_fully_history %>% 
#   pivot_wider(names_from = "ticker", values_from = "return_adj_close") %>%
#   arrange(timestamp) %>%
#   data.frame()
# daily_return_adjusted_data_with_fully_history_xts <- 
#   xts(daily_return_adjusted_data_with_fully_history_xts %>% select(-c("timestamp")), order.by=as.Date(daily_return_adjusted_data_with_fully_history_xts$timestamp))
# 



tickers <- daily_return_adjusted_data_with_fully_history$ticker %>% unique()

mat <- matrix(NA, ncol=length(tickers), nrow=length(unique(daily_return_adjusted_data_with_fully_history$timestamp)))
colnames(mat) <- tickers
mat <- xts(mat, order.by = sort(unique(daily_return_adjusted_data_with_fully_history$timestamp), decreasing = F))


for(i in 1:length(tickers)){
  print(i)
  tick <- tickers[i]
  temp <- daily_return_adjusted_data_with_fully_history %>% 
    filter(ticker==tick) %>% 
    arrange(timestamp)
  mat[temp$timestamp,tick] <- temp$return_adj_close
}

mat[is.na(mat)] <- 0

xts_dracfh <- mat
save(ticker_with_full_history, daily_return_adjusted_data_with_fully_history, xts_dracfh, file="world_index_data_v3.rdata")

library(plotly)
plotly_line_chart_xts(return_to_cumret(xts(xts_dracfh %*% rep(1/ncol(xts_dracfh),ncol(xts_dracfh)), order.by=index(xts_dracfh))))


max(xts_dracfh)
max_col <- which(xts_dracfh == max(xts_dracfh), arr.ind = TRUE)[2]

xts_dracfh <- xts_dracfh[,-max_col]




min(xts_dracfh)
max_col <- which(xts_dracfh == min(xts_dracfh), arr.ind = TRUE)[2]

xts_dracfh <- xts_dracfh[,-max_col]

save(xts_dracfh, file="xts_dracfh.rdata")


