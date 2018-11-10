library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
silverstar<-read_html("http://www.skisilverstar.com/my-mountain-info/mountain-conditions/snow-report")
updated<-silverstar %>% html_nodes("div.report-section p.updated") %>% html_text(trim=TRUE)
updated<-as.POSIXct(parse_date_time(updated,orders = c("dmy T","mdy T"),truncated=2,tz = "America/Vancouver"))

test<-data.table(Parameter =(silverstar %>% html_nodes("div.report-section li p.label") %>% html_text()), 
                 Value=(silverstar %>% html_nodes("div.report-section li p.data") %>% html_text()))

test[,Parameter:=str_replace_all(Parameter,"[^[:alnum:]]","")]
test[,Value:=gsub(pattern = "(-)?( +)?(cm|°C)",replacement = "",Value),]

cnames<-as.character(test$Parameter)
test$Parameter<-NULL
test<-data.table(t(test))
colnames(test)<-cnames

data.table(Time=as.POSIXct(updated),Resort="Silver Star",Date=updated,snow12h=test$Last12Hours,snow24h=test$Last24Hours,Snow48h=NA,Snow7d = test$Last7Days,Base=test$Alpine,Cum=test$Cumulative,Temp=test$CurrentTemp,
           #Weather=test$Skies,
           #Wind=test$Winds,
           Weather = NA,
           Wind = NA,
           Lifts=NA,Runs=test$AlpineRunsOpen)

