

my_spread <- function(data, date_col, key, value){
  nc <- data %>% pull(key) %>% unique() %>% length() 
  nr <- data %>% pull(date_col) %>% unique() %>% length() 
  mat <- matrix(NA, ncol = nc, nrow=nr)
  colnames(mat) <- data %>% pull(key) %>% unique()
  mat <- xts(mat, order.by = data %>% pull(date_col) %>% unique() %>% as.Date())
  for(i in 1:nc){
    if(i%%10==0){cat(paste0(i," "))}
    ticker <- colnames(mat)[i]
    temp <- data %>% ungroup() %>% filter(!!sym(key)==ticker) %>% select(any_of(c(date_col, value)))
    mat[temp %>% pull(date_col),i] <- temp %>% pull(value) %>% as.numeric()
  }
  return(mat)
}


get_data_for_ticker_alphaadvantage <- function(tickers, min_date, alpha_key, daily_prices = TRUE, daily_return = TRUE, overview = TRUE, NAV = TRUE){
  
  data <- list(
    daily_prices = list(),
    daily_return = list(),
    overview = list(),
    NAV = list()
  )
  print(" ")
  print("daily_data")
  daily_data_raw <- list()
  for(i in 1:length(tickers)){
    ticker <- tickers[i]
    if(i%%10==0){
      cat(paste0(i, " "))
    }
    
    start_time <- Sys.time()
    suppressMessages(while(as.numeric(Sys.time()-start_time) <= 62){
      try({
        temp_daily_return <- httr::content(
          httr::GET(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=",ticker,"&outputsize=full&apikey=",api_key,"&datatype=csv")), 
          as = "text", 
          encoding = "UTF-8"
        )
        
        if(substr(temp_daily_return, 1, 9) == "timestamp"){
          daily_data_raw[[i]] <- cbind(
            "ticker"=ticker,
            temp_daily_return %>% 
              read_csv() %>% 
              filter(timestamp>=min_date) %>% 
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
    })
    
  }
  
  daily_data_raw <- bind_rows(daily_data_raw)
  
  daily_data_raw <- daily_data_raw %>% 
    arrange(ticker, timestamp) %>% 
    group_by(ticker) %>% 
    mutate(
      "return_adj_close" = adjusted_close/dplyr::lag(adjusted_close, order_by = timestamp)-1
    ) %>% 
    filter(!is.na(return_adj_close))
  
  

  data$daily_return <- my_spread(data=daily_data_raw, date_col="timestamp", key="ticker", value="return_adj_close")
                        
  data$daily_prices <- my_spread(data=daily_data_raw, date_col="timestamp", key="ticker", value="adjusted_close")
  
  rm(daily_data_raw)
  
  
  print(" ")
  print("overview_data")
  overview_data_raw <- list()
  for(i in 1:length(tickers)){
    ticker <- tickers[i]
    
    if(i%%10==0){
      cat(paste0(i, " "))
    }
    
    start_time <- Sys.time()
    while(as.numeric(Sys.time()-start_time) <= 62){
      try({
        temp_overview <- data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=",ticker,"&apikey=",api_key)))
      
        if(ncol(temp_overview)>1){
          overview_data_raw[[i]] <- temp_overview
          break
        }else if(ncol(temp_overview)==1 && !"Error.Message" %in% names(temp_overview)){
          cat("+")
          Sys.sleep(1)
        }else{
          print("Not found!")
          break
        }
        
      })
    }
  }
  
  data$overview <- bind_rows(overview_data_raw)
  rm(overview_data_raw)
  
  print(" ")
  print("NAV_data")
  NAV_data_raw <- list()
  for(i in 1:length(tickers)){
    ticker <- tickers[i]
    
    if(i%%10==0){
      cat(paste0(i, " "))
    }
    
    start_time <- Sys.time()
    while(as.numeric(Sys.time()-start_time) <= 62){
      try({
        temp_nav <- jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=BALANCE_SHEET&symbol=",ticker,"&apikey=",api_key))
        temp_nav <- cbind("ticker"=ticker, temp_nav$quarterlyReports) %>% 
          select(ticker, "date"=fiscalDateEnding, totalCurrentAssets)
        
        if(ncol(temp_overview)>1){
          NAV_data_raw[[i]] <- temp_nav
          break
        }else if(ncol(temp_nav)==1 && !"Error.Message" %in% names(temp_nav)){
          cat("+")
          Sys.sleep(1)
        }else{
          print("Not found!")
          break
        }
      })
    }
  }
  
  NAV_data_raw <- bind_rows(NAV_data_raw)
  
  data$NAV <- my_spread(data=NAV_data_raw, date_col="date", key="ticker", value="totalCurrentAssets")
  rm(NAV_data_raw)
  
  return(data)
}