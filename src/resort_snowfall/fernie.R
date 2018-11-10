library("rvest")
library(data.table)
library(stringr)
library(lubridate)

#eluid4eed9cc0 > div.zn_section_size.container.zn-section-height--auto.zn-section-content_algn--top > div.row.zn_columns_container.zn_content > div > div.zn_text_box.eluida96112ec.zn_text_box-light.element-scheme--light > h4 > div

# Get Snowfall Data
fernie<-read_html("http://skifernie.com/conditions/snow-report/")
updated<-paste((fernie %>% html_nodes("div.zn_text_box.eluida96112ec.zn_text_box-light.element-scheme--light h4 div") %>% html_text(trim=TRUE))[1],
               fernie %>% html_nodes(".rcr-element-box .rcr-element-box-datetime") %>% html_text(trim=TRUE))

updated<-parse_date_time(updated[1],orders = c("mdy T","mdy R","md R"),truncated=3,tz = "America/Edmonton")

test <- c(fernie %>% html_nodes(".rcr-element-box-fl16") %>% html_text(trim=TRUE),fernie %>% html_nodes(".rcr-element-box-fl16") %>% html_text(trim=TRUE))

# Set "Trace" to zeroes!
test <- gsub(pattern = "Trace|trace",replacement = "0",ignore.case = TRUE, x= test)

data.table(Time=updated,
           Resort = "Fernie",
           Date = updated,
           snow12h = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMOVERNIGHT)",test,perl = TRUE)))[1],
           snow24h = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMLAST 24 HOURS)",test,perl = TRUE)))[1],
           Snow48h = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMLAST 48 HOURS)",test,perl = TRUE)))[1],
           Snow7d = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMLAST 7 DAY)",test,perl = TRUE)))[1],
           Base = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMBASE)",test,perl = TRUE)))[1],
           Cum = as.numeric(regmatches(test,regexpr("[0-9]+(?= CMTHIS SEASON)",test,perl = TRUE)))[1],
           Temp = NA,
           Weather = NA,
           Wind = NA,
           Lifts = NA,
           Runs = as.numeric(regmatches(test,regexpr("[0-9]+( )?(?= OPEN RUNS)",test,perl = TRUE)))[1]
)
