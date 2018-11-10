library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
panorama<-read_html("https://www.panoramaresort.com/panorama-today/daily-snow-report/")
updated<-(panorama %>% html_nodes("div.summary-current.margin-bottom-2 p.small.light-text") %>% html_text(trim=TRUE))[1]
updated<-parse_date_time(updated,orders = c("Ip dmy","R dmy"),truncated=2,tz = "America/Edmonton")

test<-(panorama %>% html_nodes("div.grid-x.grid-margin-x.margin-bottom-1") %>% html_text(trim=TRUE))
test <- str_replace_all(test,"24 Hours","TFh")
test <- str_replace_all(test,"48 Hours","FEh")
test <- str_replace_all(test,"7 Days","Sd")
test <- str_replace_all(test,"\n","")
test <- gsub("^ *|(?<= ) | *$|\n", "", test, perl = TRUE)

test <- unlist(str_split(test," "))

is.odd <- function(x) x%%2 !=0

test2 <- data.table(snow = test[is.odd(seq_along(test))], time = test[!is.odd(seq_along(test))])

test2[,snow:=as.numeric(str_replace_all(snow,"[[:alpha:]]",""))]

snow <- data.frame(test2)

cn <- snow$time
snow$time <- NULL
snow <- data.frame(t(snow))
colnames(snow) <- cn

data.table(Time=updated,Resort="Panorama",Date=updated,snow12h=snow$Overnight,snow24h=snow$TFh,Snow48h=snow$FEh,Snow7d = snow$Sd,Base=snow$Summit,Cum=snow$Season,Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

