library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
sunpeaks<-read_html("https://www.sunpeaksresort.com/ski-ride/weather-conditions-cams/weather-snow-report")
updated<-(sunpeaks %>% html_nodes("div#snow-conditions p.mice-type") %>% html_text(trim=TRUE))
updated <- regmatches(updated,regexec(pattern = "(?<=Updated ).+$",updated,perl=TRUE))[[1]]
updated <- parse_date_time(updated,orders = ("md R"),truncated=2,tz = "America/Vancouver")

test <- gsub(pattern = "[[:space:]]+"," ", sunpeaks %>% html_nodes("div#snow-conditions ul.list-snow") %>% html_text(trim=TRUE))
test <- unlist(str_split(test,"cm"))
test <- paste0(test,ifelse(str_length(test)>0,"cm",""))

test <- data.table(
  Time = tolower(str_replace_all(unlist(regmatches(test,regexec(".+(?= [[:digit:]])",test,perl=TRUE))),"[^[:alnum:]]","")),
  Snowfall = as.numeric(unlist(regmatches(test,regexec("[[:digit:]]+(?= cm)",test,perl=TRUE))))
)

cnames<-as.character(test$Time)
test$Time<-NULL
test<-data.table(t(test))
setnames(test,cnames)

data.table(Time=updated,Resort="Sun Peaks",Date=updated,snow12h=test$newsnow,snow24h=test$`24hr`,Snow48h=test$`48hr`,Snow7d = test$`7days`,Base=test$alpine,Cum=NA,Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

