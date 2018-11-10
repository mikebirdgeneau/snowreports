library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
castle<-read_html("http://www.skicastle.ca/snow-report-conditions/")
updated <- gsub(pattern = "[[:space:]]+"," ",castle %>% html_nodes("div.container div.col-sm-3") %>% html_text(trim=TRUE))
updated <- regmatches(updated,regexpr("(?<=Conditions as of).+$",updated,perl=TRUE))
updated<-parse_date_time(updated,orders = ("mdy R"),truncated=2,tz = "America/Edmonton")

test<-data.table((castle %>% html_nodes("table.table-snow") %>% html_table(fill=TRUE,header=TRUE))[[1]])

setnames(test,c("Time","Snowfall","Village"))

test[,Snowfall:=as.numeric(str_replace_all(str_replace_all(Snowfall,"[^[:alnum:]]",""),"[[:alpha:]]","")),]
test[,Time:=tolower(str_replace_all(Time,"[^[:alnum:]]","")),]

test<-data.table(t(test))
setnames(test,as.character(unlist(test[1,])))
test<-test[2,]

data.table(Time=updated,Resort="Castle",Date=updated,snow12h=NA,snow24h=test$freshsnow24hrs,Snow48h=test$freshsnow48hrs,Snow7d = test$last7days,Base=test$snowbase,Cum=test$totalsnowfall,Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

