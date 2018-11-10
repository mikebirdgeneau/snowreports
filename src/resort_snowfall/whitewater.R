library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
whitewater<-read_html("https://www.skiwhitewater.com/ski-ride/conditions/snow-report")
updated <- (whitewater %>% html_nodes("div.node-snow-report div.col-md-4") %>% html_text(trim=TRUE))
updated <- regmatches(updated,regexpr("(?<=Report Updated at:).+",text = updated, perl=TRUE))
updated<-parse_date_time(paste(updated,collapse = " "),orders = c("mdy R","mdy T"),truncated=2,tz = "America/Vancouver")

test<-data.table(((whitewater %>% html_nodes("div.snow-report-inner-block table")) %>% html_table(fill=TRUE,header=TRUE))[[1]])

cnames <- colnames(test)
test <- test[,unlist(lapply(.SD, function(x){as.numeric(gsub(pattern = " ","",regmatches(x,regexec("[[:digit:]]{1,3}[[:space:]]{0,5}(?=cm)",x,perl = TRUE))))})),.SDcols=colnames(test)]
test <- data.table(t(test))

total <- gsub(pattern = "[[:space:]]","",x=whitewater %>% html_nodes("span.total-snow") %>% html_text())
total <- regmatches(total,regexpr(pattern = "(?<=TotalSnow)[[:digit:]]{1,4}(?=cm)",total,perl=TRUE))

base <-  gsub(pattern = "[[:space:]]","",x=whitewater %>% html_nodes("span.total-snowbase") %>% html_text())
base <- regmatches(base,regexpr(pattern = "(?<=SettledSnowpack)[[:digit:]]{1,4}(?=cm)",base,perl=TRUE))

data.table(
  Time=updated,
  Resort="Whitewater",
  Date=updated,
  snow12h=test$Overnight,
  snow24h=test$`24 Hrs`,
  Snow48h=test$`48 hrs`,
  Snow7d = test$`7 Days`,
  Base=base,
  Cum=total,
  Temp=NA,
  Weather=NA ,
  Wind=NA,
  Lifts=NA,
  Runs=NA)
