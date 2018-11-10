library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
apex<-read_html("https://www.apexresort.com/the-mountain/snow-report/")
test <- data.table((apex %>% html_nodes("table.sr_table") %>% html_table(fill=TRUE,header=NA,trim=TRUE))[[1]])
setnames(test,c(paste0("c",seq(1,ncol(test),by=1))))
test<-data.table(t(test))
setnames(test,str_trim(as.character(unlist(test[1,]))))
test<-data.table(test[2,])

setnames(test,c("New Snow(in the past 12 hours)"),c("Snow12h"))
setnames(test,c("New Snow(in the past 24 hours)"),c("Snow24h"))
setnames(test,c("New Snow(in the past 48 hours)"),c("Snow48h"))
setnames(test,c("New Snow(in the past 7 days)"),c("Snow7d"))
setnames(test,c("Alpine Base CM"),c("Base"))


test$Temperature<-as.numeric(regmatches(test$Temperature,gregexpr('[0-9]+(\\.[0-9]+)?',test$Temperature))[[1]][1])
test$Snow12h<-as.numeric(regmatches(test$Snow12h,gregexpr('[0-9]+(\\.[0-9]+)?',test$Snow12h))[[1]][1])
test$Snow24h<-as.numeric(regmatches(test$Snow24h,gregexpr('[0-9]+(\\.[0-9]+)?',test$Snow24h))[[1]][1])
test$Snow48h<-as.numeric(regmatches(test$Snow48h,gregexpr('[0-9]+(\\.[0-9]+)?',test$Snow48h))[[1]][1])
test$Snow7d<-as.numeric(regmatches(test$Snow7d,gregexpr('[0-9]+(\\.[0-9]+)?',test$Snow7d))[[1]][1])
test$Base<-as.numeric(regmatches(test$Base,gregexpr('[0-9]+(\\.[0-9]+)?',test$Base))[[1]][1])


# Get Last Updated
updated<-parse_date_time(paste(test$Date,test$Time),orders = c("ymd R","ymd Ip","mdy R","mdy Ip"),truncated=2,tz = "America/Vancouver")

data.table(Time=updated,Resort="Apex",Date=updated,snow12h=test$Snow12h,snow24h=test$Snow24h,Snow48h=test$Snow48h,Snow7d = test$Snow7d,Base=test$Base,Cum=NA,Temp=NA,Weather=test$Conditions,Wind=test$Wind,Lifts=test$`Lifts Open`,Runs=test$`Trails Open`)

