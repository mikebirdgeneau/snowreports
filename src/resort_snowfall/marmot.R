library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
marmot<-read_html("http://www.skimarmot.com/u/conditions_snow.phtml")
updated<-marmot %>% html_nodes("div.da1-da4 h2") %>% html_text(trim=TRUE)
updated<-parse_date_time(updated,orders = ("dm R"),truncated=2,tz = "America/Edmonton")

test<-data.table((marmot %>% html_nodes("div.da5-da12 table") %>% html_table())[[1]])
cnames<-colnames(test)
test<-data.table(t(test))
test$Time<-cnames
test[,Snowfall:=as.numeric(str_replace_all(test$V1,pattern = "[[:alpha:]]",""))]
test$V1<-NULL

test<-data.table(t(test))
setnames(test,as.character(unlist(test[1,])))
test<-test[2,]

data.table(Time=updated,Resort="Marmot",Date=updated,snow12h=NA,snow24h=test$`24 Hours`,Snow48h=NA,Snow7d = NA,Base=test$Base,Cum=test$`Season Total`,Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

