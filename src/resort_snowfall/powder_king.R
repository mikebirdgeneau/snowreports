library("rvest")
library(data.table)
library(stringr)
library(reshape2)
library("lubridate")

# Get Snowfall Data
pk<-read_html("https://ski.powderking.com/ski-report/")
test <- data.table((pk %>% html_nodes("div.content table.stripe") %>% html_table(fill=TRUE,header=TRUE,trim=TRUE))[[1]])
suppressWarnings(colnames(test)[1] <- "Location")
test <- data.table(melt(test,id.vars = "Location"))
setnames(test,c("variable","value"),c("Time","Snowfall"))
test[,Snowfall:=as.numeric(gsub(pattern = "[^[:digit:]]", replacement = "",Snowfall)),]

# Get Last Updated
pk.updated<-(pk %>% html_nodes("div.content p em") %>% html_text)
pk.updated<-pk.updated[grepl("updated",x = pk.updated)][1]
pk.updated<-parse_date_time(pk.updated,orders = c("Rmdy","Hp mdy","mdy"),truncated=2,tz = "America/Vancouver")
#pk.updated <- as.POSIXct(pk.updated,format="%Y-%m-%d %H:%i:%s")

data.table(
  Time=pk.updated,
  Resort="Powder King",
  Date=pk.updated,
  snow12h=NA,
  snow24h = test[Time=="24 Hour" & Location=="Top T-Bar",Snowfall,],
  Snow48h = test[Time=="48 Hour" & Location=="Top T-Bar",Snowfall,],
  Snow7d = NA,
  Base = test[Time=="Base" & Location=="Top T-Bar",Snowfall,],
  Cum=NA,
  Temp=NA,
  Weather=NA,
  Wind=NA,
  Lifts=NA,
  Runs=NA
)
