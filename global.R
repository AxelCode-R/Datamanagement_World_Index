
options(scipen=999)
options(stringsAsFactors = FALSE)
options(max.print=100)



library(alphavantager)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(xts)

api_key <- "W59FBR90IRDZ8LVX"

for(src in list.files("R")){
  source(paste0("R/",src))
}

