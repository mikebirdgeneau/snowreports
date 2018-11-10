library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
mtwash<-read_html("https://www.mountwashington.ca/weather/alpine-snow-report.html")
updated<-(mtwash %>% html_nodes("div.span6 div.sp-block, p") %>% html_text(trim=TRUE))
updated<-regmatches(updated,regexpr("(?<=Updated:).+$",updated,perl = TRUE))[1]
updated<-parse_date_time(updated,orders = c("mdy R","BdyR","bdyR"),truncated=4,tz = "America/Vancouver")

snownodes <- (mtwash %>% html_nodes("div.span12 p") %>% html_text(trim=TRUE))
snownodes <- str_replace_all(snownodes,pattern = "\t|\n|\r","")

snowbase <- regmatches(snownodes,regexpr("(?<=(base|Base)).+(?=cm).+$",snownodes,perl=TRUE))
snowbase<-as.numeric(str_replace_all(regmatches(snowbase,regexpr("[[:digit:]]+(cm)",text = snowbase)),pattern = "cm",""))
# Overnight Snowfall
overnightsnowfall <- regmatches(snownodes,regexpr("(?<=(overnight|Overnight)).+(?=cm).+$",snownodes,perl=TRUE))
overnightsnowfall<-as.numeric(regmatches(overnightsnowfall,regexpr("(-)?[[:digit:]]+",text = overnightsnowfall)))

snow24h <- regmatches(snownodes,regexpr("(?<=(24 Hour)).+(?=cm).+$",snownodes,perl=TRUE))
snow24h<-as.numeric(regmatches(snow24h,regexpr("(-)?[[:digit:]]+",text = snow24h)))

snow48h <- regmatches(snownodes,regexpr("(?<=(48 Hour)).+(?=cm).+$",snownodes,perl=TRUE))
snow48h<-as.numeric(regmatches(snow48h,regexpr("(-)?[[:digit:]]+",text = snow48h)))

#weather <- regmatches(snownodes,regexpr("(?<=Weather).+(?=\\r\\n)",snownodes,perl=TRUE))
#weather <- gsub("[|]",replacement = "",x = weather)

wind <- regmatches(snownodes,regexpr("Wind.+(?=\\r\\n)",snownodes,perl=TRUE))[1]

result <- data.table(Time=updated,Resort="Mt. Washington",Date=updated,snow12h=overnightsnowfall,snow24h=snow24h,Snow48h=snow48h,Snow7d = NA,Base=snowbase,Cum=NA,Temp=NA,Weather=NA ,Wind=wind,Lifts=NA,Runs=NA)

if(nrow(result)>1){
  result <- result[2,]
}
result

