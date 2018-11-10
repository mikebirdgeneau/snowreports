library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
monttremblant<-read_html("https://www.tremblant.ca/snow-weather-report")
updated<-(monttremblant %>% html_nodes("span.last-updated") %>% html_text(trim=TRUE))[1]
updated<-parse_date_time(paste(updated,collapse = " "),orders = c("R mdy","mdy R","mdy T"),truncated=2,tz = "America/Montreal")

test <- (monttremblant %>% html_nodes("div.snowfall-wrapper div.recent-snowfall li.data-point")) %>% html_text(trim=TRUE)
test <- gsub(pattern = "[^[:alnum:]]","",test)

test <- data.table(
  Time = regmatches(test,regexpr("(?<=cm).+",text = test,perl = TRUE)),
  Snowfall = as.numeric(regmatches(test,regexpr("[[:digit:]]+(?=cm)",text = test,perl = TRUE)))
)

cnames <- test$Time
values <- test$Snowfall
test <- data.table(t(values))
setnames(test,cnames)


data.table(
  Time=updated,
  Resort="Mont Tremblant",
  Date=updated,
  snow12h=test$Overnight,
  snow24h=test$`24hr`,
  Snow48h=test$`48hr` ,
  Snow7d = test$`7day`, 
  Base=NA,
  Cum=NA,
  Temp=NA,
  Weather=NA,
  Wind=NA,
  Lifts=NA,
  Runs=NA
)

