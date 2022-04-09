options(scipen=999)
options(stringsAsFactors = FALSE)
options(max.print=100)



library(alphavantager)
library(dplyr)
library(readr)
library(openxlsx)
library("jsonlite")


cboe_data <- NULL
for(file in list.files("cboe symbols/", full.names = T)){
  cboe_data <- bind_rows(
    cboe_data,
    read.csv(file)
  )
}

cboe_data <- cboe_data %>% 
  mutate(
    Name = substr(Name, 1, nchar(Name)-1)
  )

tickers <- cboe_data$Name %>% unique()

load("C:/Users/Axel/Desktop/Master-Thesis-All/Datamanagement_World_Index/world_index_data.rdata")

tickers <- tickers[!tickers %in% ticker_data$Symbol]


api_key <- "W59FBR90IRDZ8LVX"


daily_return_adjusted_data <- list()
overview_data <- list()
for(i in (min(length(daily_return_adjusted_data), length(overview_data))+1):length(tickers)){
  ticker <- tickers[i]
  
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
tickers <- tickers



