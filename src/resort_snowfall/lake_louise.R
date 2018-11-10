library("rvest")
library(data.table)
library(stringr)

options(stringsAsFactors = FALSE)

# Get Snowfall Data
lakelouise<-read_html("http://www.skilouise.com/conditions/snow-report.php")
updated<-lakelouise %>% html_nodes("div.srWrap div.srThreeColOne") %>% html_text(trim=TRUE)
updated<-parse_date_time(updated,orders = ("mdy R"),truncated=2,tz = "America/Edmonton")

test<-data.table((lakelouise %>% html_nodes("div.srWrap div.backBowl table") %>% html_table())[[1]])
cnames<-as.character(test$`X1`)
test[,X2:=as.numeric(str_replace_all(test$X2,pattern = "[[:alpha:]]",""))]

test<-data.table(t(test))
setnames(test,tolower(str_replace_all(cnames,"[^[:alnum:]]","")))
test<-test[2,]

test[is.na(last24hrs),last24hrs:=as.numeric(0),]

cond<-str_replace_all(lakelouise %>% html_nodes("div.srWrap div.srThreeColTwo") %>% html_text(trim=TRUE),"(\\r|\\n|(\\s\\s))+"," ")
lifts<-as.numeric(lakelouise %>% html_nodes("div.srWrap div.liftsRuns2 h6") %>% html_text(trim=TRUE))
runs<-as.numeric(lakelouise %>% html_nodes("div.srWrap div.liftsRuns3 h6") %>% html_text(trim=TRUE))



data.table(Time=updated,Resort="Lake Louise",Date=updated,snow12h=NA,snow24h=test$last24hrs,Snow48h=ifelse("last48hrs" %in% colnames(test),test$last48hrs,NA),Snow7d = test$last7days,Base=test$snowdepth,Cum=test$yeartodate,Temp=NA,Weather=cond ,Wind=NA,Lifts=lifts,Runs=runs)

