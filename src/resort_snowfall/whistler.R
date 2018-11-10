library("rvest")
library(data.table)
library(stringr)
library("jsonlite")
library("lubridate")

# Get Snowfall Data
system("bin/phantomjs --ignore-ssl-errors=yes --web-security=false --debug=true src/resort_snowfall/whistler.js")

whistler <- read_html("/tmp/whistler.html")
unlink("/tmp/whistler.html")

whistler_json <- RCurl::getURL("https://www.whistlerblackcomb.com/php/website/weather_tom.php",ssl.verifypeer = FALSE)
whistler <- fromJSON(whistler_json)

snow12h <- whistler %>% html_node("#Last12Hours") %>% html_text()
snow24h <- whistler %>% html_node("#Last24Hours") %>% html_text()
snow48h <- whistler %>% html_node("#Last48Hours") %>% html_text()
snow7d <- whistler %>% html_node("#Last7Days") %>% html_text()
base <- whistler %>% html_node("#tomBase") %>% html_text()
cum <- whistler %>% html_node("#CumulativeSnow") %>% html_text()
temp <- whistler %>% html_node("#stationTemp2") %>% html_text()

data.table(Time=parse_date_time(whistler$Date$tomupdated,orders=c("mdY T"),tz="America/Vancouver"),
           Resort="Whistler",
           Date=parse_date_time(whistler$Date$tomupdated,orders=c("mdY T"),tz="America/Vancouver"),
           snow12h=as.numeric(regmatches(snow12h,regexpr(pattern = "[[:digit:]](?=cm)",snow12h,perl = TRUE))),
           snow24h=as.numeric(regmatches(snow24h,regexpr(pattern = "[[:digit:]](?=cm)",snow24h,perl = TRUE))),
           Snow48h=as.numeric(regmatches(snow48h,regexpr(pattern = "[[:digit:]](?=cm)",snow48h,perl = TRUE))),
           Snow7d=as.numeric(regmatches(snow7d,regexpr(pattern = "[[:digit:]](?=cm)",snow7d,perl = TRUE))),
           Base=as.numeric(regmatches(base,regexpr(pattern = "[[:digit:]](?=cm)",base,perl = TRUE))),
           Cum=as.numeric(regmatches(cum,regexpr(pattern = "[[:digit:]](?=cm)",cum,perl = TRUE))),
           Temp=as.numeric(regmatches(cum,regexpr(pattern = "[[:digit:]](?=°C)",cum,perl = TRUE))),
           Weather=NA,
           Wind=NA,
           Lifts=NA,
           Runs=NA
           )

