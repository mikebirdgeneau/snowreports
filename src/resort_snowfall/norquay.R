library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
norquay<-read_html("http://winter.banffnorquay.com/conditions/")
updated<-(norquay %>% html_nodes("#post-area p") %>% html_text(trim=TRUE))[1:4]
updated<-suppressWarnings(parse_date_time(updated,orders = ("mdy R"),truncated=2,tz = "America/Edmonton"))
updated<-updated[which(!is.na(as.POSIXct(updated)))][1]

# If updated is today, then add hour...
if(as.Date(updated)==Sys.Date()){
  updated<-updated+hour(Sys.time())*3600+minute(Sys.time())*60
} else {
  updated<-updated+20*3600
}

test<-data.table((norquay %>% html_nodes("div.nectar-milestone") %>% html_text()))
test[,parameter:=str_trim(str_match(str_trim(test$V1),"(-?[0-9]+(\\.)?([0-9]+)?)(.+)")[,5]),]
test[,value:=str_trim(str_match(str_trim(test$V1),"(-?[0-9]+(\\.)?([0-9]+)?)(.+)")[,2]),]
test$V1<-NULL
cnames<-test$parameter

test<-data.table(t(test))
setnames(test,tolower(str_replace_all(cnames,"[^[:alnum:]]","")))
test<-test[2,]

data.table(Time=as.POSIXct(updated),Resort="Norquay",Date=updated,snow12h=NA,snow24h=test$newsnowlast24hours,Snow48h=ifelse(is.null(test$newsnowlast48hours),NA,test$newsnowlast48hours),Snow7d = test$newsnowlast7days,Base=NA,Cum=test$yeartodatesnowfall,Temp=test$currenttemperature,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

