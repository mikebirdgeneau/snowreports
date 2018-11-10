library("rvest")
library(data.table)
library(stringr)
library("methods")
library(lubridate)
library(tesseract)
library(rPython)
options(stringsAsFactors = FALSE)
options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36")



# Get Snowfall Data
system("python src/resort_snowfall/sunshine.py")
sunshine <- read_html("/tmp/sunshine.html")
unlink("/tmp/sunshine.html")

updated<-sunshine %>% html_nodes("section#currentConditions div.updatedAt p") %>% html_text(trim=TRUE)
updated<-parse_date_time(updated,orders = ("mdy R"),truncated=2,tz = "America/Edmonton")

snow <- sunshine %>% html_nodes("div.currentConditionsContent div.currentCondition") %>% html_text(trim=TRUE)
snow <- str_replace_all(unlist(str_split(snow,"\n"))," ","")

is.odd <- function(x) x%%2 !=0

snow <- data.table(times = snow[is.odd(seq_along(snow))], snow = snow[!is.odd(seq_along(snow))])

snow[,snow:=regmatches(snow,regexpr(pattern = "^[0-9]+(?=cm)",text = snow,perl = TRUE))]

data.table(Time=updated,Resort="Sunshine",Date=as.Date(updated),
            snow12h=snow[times=="Overnight",snow],
            snow24h=snow[times=="24Hours",snow],
            Snow48h=NA,
            Snow7d = snow[times=="7Days",snow],
            Base=snow[times=="SettledBase",snow],
            Cum=snow[times=="Cumulative",snow],
            Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

# 
# # Get conditions 'image' 
# img <- sunshine %>% html_nodes("#hero div.heroContent div div.floatingImage img") %>% html_attr("src")
# img <- paste0("https://www.skibanff.com",img)
# tmpfile <- tempfile(pattern="sunshine_",fileext = ".jpg")
# download.file(img,destfile = tmpfile,method="curl")
# system(paste0("convert -units PixelsPerInch ",tmpfile," -resample 300 -density 300 -unsharp 10x4+1+0 -threshold 70% -morphology Erode Octagon -negate  ",tmpfile,".tiff"))
# 
# text <- ocr(paste0(tmpfile,".tiff"),
#             engine = tesseract('eng', options = list(
#               tessedit_char_blacklist = 'Ge',
#               tessedit_pageseg_mode = 'auto',
#               textord_tabfind_find_tables = '0',
#               textord_tablefind_recognize_tables = '0',
#               segment_adjust_debug = '1',
#               load_system_dawg = '0',
#               load_freq_dawg = '0'
#             )))
# try({
#   unlink(tmpfile)
#   unlink(paste0(tmpfile,".tiff"))
# })

#text <- str_replace_all(text,"em","cm")
#text <- str_replace_all(text,"Gcm","0cm")
# 
# updated <- str_replace_all(regmatches(img,regexpr("_[A-Z]{3}[0-9]{1,2}.+\\.jpg$",text = img,perl=TRUE)),"_|\\.jpg","")
# updated <- parse_date_time(paste0(updated," 2017",strftime(Sys.time(),"%H:%M")),orders=c("mdY R"),tz="America/Edmonton")
# 
# text <- str_split(text,"\n")
# snow <- lapply(text,function(x){regmatches(x,gregexpr("[0-9]{1,3}cm",x))})
# 
# snow <- data.table(times = c("Overnight","24 Hours","7 Days","Settled Base","Cumulative"),
#            snow = c(snow[[1]][[2]][1],snow[[1]][[2]][2],snow[[1]][[3]][1],snow[[1]][[3]][2],snow[[1]][[3]][3]))
# snow[,num:=as.numeric(str_replace_all(snow,"cm","")),]
# 
# data.table(Time=updated,Resort="Sunshine",Date=as.Date(updated),
#            snow12h=snow[times=="Overnight",num],
#            snow24h=snow[times=="24 Hours",num],
#            Snow48h=NA,
#            Snow7d = snow[times=="7 Days",num],
#            Base=snow[times=="Settled Base",num],
#            Cum=snow[times=="Cumulative",num],
#            Temp=NA,Weather=NA ,Wind=NA,Lifts=NA,Runs=NA)

