library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
bigwhite<-read_html("https://www.bigwhite.com/mountain-conditions/daily-snow-report")
updated<-bigwhite %>% html_nodes("section.block-snowreportblock div.small-12") %>% html_text()
updated <- updated[grepl(updated,pattern = ".+Last Updated.+")]
updated<-parse_date_time(updated,orders = ("mdy R"),truncated=2,tz = "America/Vancouver")

test <- bigwhite %>% html_nodes(".small-6") %>% html_text(trim=TRUE)
test <- gsub("\n"," ",x=gsub("\n( )+",replacement = "\n",x = test))

snow <- regmatches(x = test,regexpr(pattern = ".+[[:digit:]].+( )?cm",text = test,perl = TRUE))
snow <- gsub(pattern = "[[:space:]]+",replacement = " ",snow)
snow <- gsub(pattern = "\\(|\\)",replacement = "",snow)

data.table(
  Time=updated,
  Resort="Big White",
  Date=updated,
  snow12h=as.numeric(gsub(pattern = "[^[:digit:]]",replacement = "",x = regmatches(test,regexpr("^[0-9]{1,3}.+(?=cm)",test,perl = TRUE)),perl = TRUE))[1],
  snow24h=as.numeric(regmatches(snow,regexpr("(?<=New Snow 24hrs )[[:digit:]]{1,3}",snow,perl=TRUE))),
  Snow48h=NA,
  Snow7d = as.numeric(regmatches(snow,regexpr("(?<=New Snow 7 Days )[[:digit:]]{1,3}",snow,perl=TRUE))),
  Base=as.numeric(regmatches(snow,regexpr("(?<=Alpine snow Base )[[:digit:]]{1,3}",snow,perl=TRUE))),
  Cum=as.numeric(regmatches(snow,regexpr("(?<=Cumulative snowfall )[[:digit:]]{1,3}",snow,perl=TRUE))),
  Temp=as.numeric(regmatches(test,regexpr("(?<=Weather)( ){0,5}(-)?[[:digit:]]{1,3}(?=℃)",test,perl = TRUE))),
  Weather=regmatches(test,regexpr("Wind.+",test,perl = TRUE))[1],
  Wind=NA,
  Lifts=as.numeric(regmatches(test,regexpr("(?<=Lift Open)( ){0,5}(-)?[[:digit:]]{1,3}",test,perl = TRUE)))[1],
  Runs=as.numeric(regmatches(test,regexpr("(?<=Runs Open)( ){0,5}(-)?[[:digit:]]{1,3}",test,perl = TRUE)))[1]
           )

