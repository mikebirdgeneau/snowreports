library("rvest")
library(data.table)
library(stringr)
library(lubridate)

# Get Snowfall Data - Red seems to have a funny HTTP response (XML header, with GZIP data!?)
red <- read_html("http://www.redresort.com/mountain/report/")

updated<-(red %>% html_nodes("div.snowreport-half div.noteBox span strong") %>% html_text(trim=TRUE))
updated<-parse_date_time(updated,orders = ("mdy R"),truncated=2,tz = "America/Edmonton")

test<-data.table((red %>% html_nodes("div.half-second table.pricing-table") %>% html_table(fill=TRUE))[[1]])
test<-data.table(t(test))
test$V3<-NULL
colnames(test)<-c("Parameter","Value")

test2<-data.table((red %>% html_nodes("div.half-second table.pricing-table") %>% html_table(fill=TRUE))[[3]])
colnames(test2)<-c("Parameter","Value")

test<-rbindlist(list(test,test2))

test[,Value:=regmatches(Value,regexpr("[[:digit:]]+(\\.)?([[:digit:]]+)?(cm)",text = Value)),]
test[,Value:=str_replace_all(Value,pattern = "[[:alpha:]]","")]

test[,Parameter:=str_replace_all(Parameter,"[[:punct:]]|[[:space:]]","")]

cnames<-test$Parameter
test<-data.table(t(test))
colnames(test)<-cnames
test<-test[2,]

data.table(Time=updated,Resort="Red Mountain",Date=updated,snow12h=test$Overnight3pm9am,snow24h=test$`24Hours`,Snow48h=test$`48Hours`,Snow7d = test$`7Days`,Base=test$CurrentAlpineSnowDepth,Cum=test$TotalsnowfallsinceOct1st,Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

