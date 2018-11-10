library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
whitefish<-read_html("https://skiwhitefish.com/snowreport/")
updated<-(whitefish %>% html_nodes("div.entry-content div.row div.col-sm-3 h3") %>% html_text(trim=TRUE))
updated<-parse_date_time(paste(updated,collapse = " "),orders = c("mdy R","mdy T"),truncated=2,tz = "America/Denver")

test<-(whitefish %>% html_nodes("div.row div.col-sm-6") %>% html_text(trim=TRUE))
test<-str_split(paste0(test,collapse = "\n"),"\n")[[1]]

test<-rbindlist(lapply(str_split(test,": "),function(x){
  data.table(description=x[1],value=x[2])
}))

test[,description:=str_replace_all(description,"\t",""),]
test[grepl("[[:digit:]]+cm$",value),value:=regmatches(value,regexpr("[[:digit:]]+cm$",text = value)),]
test[grepl("[[:digit:]]+&degC$",value),value:=regmatches(value,regexpr("[[:digit:]]+&degC$",text = value)),]
test[grepl("[[:digit:]]+°C$",value),value:=regmatches(value,regexpr("[[:digit:]]+°C$",text = value)),]
test[grepl("[[:digit:]]",value),str_replace_all(value,"[^[:alnum:]]","")]
test[,value:=str_replace_all(value,"cm",""),]
test[,value:=str_replace_all(value,"&degC",""),]
test[,value:=str_replace_all(value,"°C",""),]

data.table(
  Time=updated,
  Resort="Whitefish",
  Date=updated,
  snow12h=NA,
  snow24h=as.numeric(test[description=="24hr Snow",value]),
  Snow48h=NA,
  Snow7d = as.numeric(test[description=="7 Day Snow",value]),
  Base=as.numeric(test[description=="Village Settled",value]),
  Cum=as.numeric(test[description=="Total to Date",value]),
  Temp=as.numeric(test[description=="Temperature",value]),
  Weather=test[description=="Current Conditions",value],
  Wind=test[description=="Visibility/Wind",value],
  Lifts=NA,
  Runs=NA)[1,]

