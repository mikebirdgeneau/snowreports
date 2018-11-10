library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
revelstoke<-read_html("http://www.revelstokemountainresort.com/conditions/snow-report")
updated<-revelstoke %>% html_nodes("section.snow-report p.omega strong") %>% html_text(trim=TRUE)
updated<-parse_date_time(updated,orders = ("mdy R"),truncated=2,tz = "America/Vancouver")

test<-data.table(revelstoke %>% html_nodes("section.snow-report div.four") %>% html_text())

regmatches(test$V1[2],regexpr("[[:digit:]]+(\\.)?([[:digit:]]+)?( +)?cm",test$V1[2]))

regmatches(test$V1,regexpr("[[:digit:]]+(\\.)?([[:digit:]]+)?( +)?cm",text = test$V1))

test[,cm:=regmatches(V1,regexpr("[[:digit:]]+(\\.)?([[:digit:]]+)?( +)?cm",text = V1)),by=c("V1")]
test[,Parameter:=str_replace_all(str_replace_all(V1,"cm|in|NA",""),"[^[:alpha:]]","")]
test[,cm:=gsub(pattern = "( +)?cm",replacement = "",cm),]

test$V1<-NULL
cnames<-test$Parameter
test$Parameter<-NULL
test<-data.table(t(test))
colnames(test)<-tolower(str_replace_all(cnames,"[^[:alnum:]]",""))

data.table(Time=updated,Resort="Revelstoke",Date=updated,snow12h=test$newsnowresetpm,snow24h=test$hours,Snow48h=NA,Snow7d = test$days,Base=test$basedepthm,Cum=test$seasontotal,Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

