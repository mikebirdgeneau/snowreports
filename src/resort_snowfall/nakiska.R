library("rvest")
library(data.table)
library(stringr)
library(lubridate)

#eluid4064b387 > div > div > div > div.zn_text_box.eluidd4a517ff.zn_text_box-light.element-scheme--light > h4 > div

#eluid4064b387 > div > div > div > div.zn_text_box.eluid0da0e911.zn_text_box-light.element-scheme--light > h4 > div

# Get Snowfall Data
nakiska<-read_html("http://skinakiska.com/conditions/snow-report/")
updated<-paste((nakiska %>% html_nodes("div.zn_text_box.eluid0da0e911.zn_text_box-light.element-scheme--light h4 div") %>% html_text(trim=TRUE))[1],
               nakiska %>% html_nodes(".rcr-element-box .rcr-element-box-datetime") %>% html_text(trim=TRUE))

updated<-parse_date_time(updated[1],orders = c("mdy T","mdy R","md R"),truncated=3,tz = "America/Edmonton")

#eluid4064b387 > div > div > div > div:nth-child(10) > div:nth-child(1)

test <- nakiska %>% html_nodes(".rcr-element-box-fl16") %>% html_text(trim=TRUE)

# Set "Trace" to zeroes!
test <- gsub(pattern = "Trace|trace",replacement = "0",ignore.case = TRUE, x= test)

data.table(Time=updated,
           Resort = "Nakiska",
           Date = updated,
           snow12h = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMOVERNIGHT)",test,perl = TRUE))),
           snow24h = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMLAST 24 HOURS)",test,perl = TRUE)))[1],
           Snow48h = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMLAST 48 HOURS)",test,perl = TRUE))),
           Snow7d = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMLAST 7 DAY)",test,perl = TRUE))),
           Base = NA,
           Cum = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMTHIS SEASON)",test,perl = TRUE))),
           Temp = NA,
           Weather = NA,
           Wind = NA,
           Lifts = NA,
           Runs = as.numeric(regmatches(test,regexpr("[0-9]+(?= OPEN RUNS)",test,perl = TRUE)))
           )
           