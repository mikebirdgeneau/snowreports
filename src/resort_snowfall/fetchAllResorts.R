library(data.table)
library(lubridate)
library(rvest)
library(ggplot2)
library(RMySQL)
library(jsonlite)
library(xtable)
library(tesseract)
options(stringsAsFactors = FALSE)

source("src/resort_snowfall/resortStatus.R")
source("lib/pushoverr.R")

resort.data<-data.table()
fetch.log<-data.table(time=Sys.time(),resort="",message="Fetching Snow Reports",type="Message")

resort.list<-c("apex","big_white","castle","fernie","kicking_horse","kimberley","lake_louise","marmot","mt_washington","nakiska","norquay","powder_king","panorama","purden","red_mountain","revelstoke","silver_star","sun_peaks","sunshine","whistler","whitewater","whitefish")

if(month(Sys.Date()) %in% c(11,12,1,2,3,4)){
  resort.list<-c("apex","big_white","castle","fernie","kicking_horse","kimberley","lake_louise","marmot","mt_washington","nakiska","norquay","powder_king","panorama","purden","red_mountain","revelstoke","silver_star","sun_peaks","sunshine","whistler","whitewater","whitefish","mont_tremblant")
} else if(month(Sys.Date()) == 5){
  resort.list<-c("lake_louise","sunshine")
} else{
  resort.list <- c()
}


for(i in 1:length(resort.list)){
  result<-tryCatch({
    result<-withCallingHandlers(source(file = paste0("src/resort_snowfall/",resort.list[[i]],".R"),local = FALSE)$value,warning=function(w){
      fetch.log<<-rbindlist(list(fetch.log,data.table(time=Sys.time(),resort=resort.list[[i]],
                                                      message=paste0(w$message,sep="\n"),type="Warning"))
      )})
    resort.data<-rbindlist(list(resort.data,result))
  },
  error=function(e){
    message(paste0("Error: ",e$message))
    fetch.log<<-rbindlist(list(fetch.log,data.table(time=Sys.time(),resort=resort.list[[i]],
                                                    message=paste0(e$message,sep="\n"),type="Error"))
    )},
  finally=message(paste("Processed",resort.list[[i]])))
}

# Convert all dates to Mountain Time (America/Edmonton)
resort.data[,Date:=format(Date,tz="America/Edmonton"),]
resort.data[,Time:=format(Time,tz="America/Edmonton"),]

# Remove any silly duplicates
setkeyv(resort.data,c("Resort"))
resort.data <- unique(resort.data)

fetch.log<-rbindlist(list(fetch.log,data.table(time=Sys.time(),resort=NA,message="Fetch Complete.",type="Message")))

if(nrow(fetch.log[type=="Error",])>0){
  #require(RPushbullet)
  require(xtable)
  require(pushoverr)
  pushover(title="Snow Fetch Error", message = paste("Error fetching snow data for: \n",paste(fetch.log[type=="Error",]$resort,sep = "\n ",collapse = "\n")))
}
subset(resort.data,select=c(Resort,Time,Date,snow24h))

#### Check for Bogus Dates, fix if needed ####
resort.data[,Time:=as.POSIXct(Time),]

resort.data[as.POSIXct(Time)>=as.POSIXct(Sys.time()+3600*7) | is.na(Time),Time:=as.POSIXct(Sys.time())] # Don't allow NA dates, or dates in the future!
resort.data[,updated:=as.POSIXct(Sys.time()),]

resort.data[as.POSIXct(Time)>updated,Time:=updated,]

#### Fix ridiculous whitespace in table ####
#resort.data[,str_replace_all(Weather,"[[:space:]]+",""),]
resort.data$Weather<-NULL

#### Clean Environment ####
rm(list=ls()[which(!c("resort.data","fetch.log") %in% ls())])

#### Connect to DB ####
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")

# Check results based on previous days data!
dt.check<-data.table(dbGetQuery(con,"SELECT id,time,resort,snow12h,snow24h,snow48h,snow7d,base,cum FROM snowdatav2 ORDER BY `id` DESC LIMIT 12960"))

# Save results into DB
dbWriteTable(con,name = "snowdatav2",data.frame(cbind(id=NA,resort.data)),append=TRUE,row.names=FALSE)

#### Process Raw Data into JSON for use in tables! ####

dt.snowdata<-data.table(dbGetQuery(con,"SELECT time,resort,snow12h,snow24h,snow48h,snow7d,base,cum,updated FROM snowdatav2 ORDER BY `id` DESC LIMIT 20000"))

dt.snowdata<-dt.snowdata[as.Date(time)==as.Date(updated),]

dt.snowdata[,time:=as.POSIXct(strptime(time,"%Y-%m-%d %H:%M:%S"),tz = "America/Edmonton"),]
dt.snowdata[,updated:=as.POSIXct(strptime(updated,"%Y-%m-%d %H:%M:%S"),tz = "America/Edmonton"),]

# Remove duplicate rows!
dt.snowdata<-dt.snowdata[order(updated,decreasing = TRUE),]
setkeyv(dt.snowdata,c("time","resort","snow12h","snow24h","snow48h","snow7d","base","cum"))
dt.snowdata<-unique(dt.snowdata)

# Get Latest Report from each Resort
dt.snowdata[,last.update:=max(time,na.rm=TRUE),by=c("resort")]
dt.snowdata[,last.updated:=max(updated,na.rm=TRUE),by=c("resort")]

dt.lastupdates<-dt.snowdata[time==last.update,]
setkeyv(dt.lastupdates,c("resort"))
dt.lastupdates[order(updated,decreasing = TRUE),]
dt.lastupdates<-unique(dt.lastupdates)
dt.lastupdates$last.update<-NULL
dt.lastupdates$last.updated<-NULL

setkeyv(dt.lastupdates,c("resort"))
#dt.lastupdates<-unique(dt.lastupdates)

writeLines(toJSON(dt.lastupdates,dataframe = "rows",factor="string",na="null",pretty=TRUE),con = "output/json/latestreports.json")


# Build table with 24h, 48h, 7d rolling numbers.
dt.snowdata[,date:=as.Date(time),]
dt.snowdata[,last.hour:=hour(last.update)+minute(last.update)/60,by=c("resort")]
dt.snowdata[,Hour:=hour(time)+minute(time)/60,]
dt.snowdata[,diff.hrs:=abs(Hour-last.hour),]

dt.snowdata[,min.diff.hrs:=min(diff.hrs,na.rm=TRUE),by=c("resort","date")]
dt.snowdata<-dt.snowdata[diff.hrs==min.diff.hrs & as.Date(time)>(as.Date(last.update)-30),]

setnames(dt.snowdata,c("snow48h","snow7d"),c("snow48h.orig","snow7d.orig"))

dt.snowdata<-dt.snowdata[order(updated,decreasing = TRUE),]
setkeyv(dt.snowdata,c("resort","date"))
dt.snowdata<-unique(dt.snowdata)

resorts<-as.list(unique(dt.snowdata$resort))
lapply(resorts,FUN = function(x){
  temp<-subset(dt.snowdata[resort==x & as.Date(time)>(as.Date(last.update)-7),],select=c(time,resort,snow12h,snow24h, snow48h.orig, snow7d.orig,base,cum))
  temp[,ReportDate:=as.Date(time,tz = "America/Edmonton"),]
  temp$time<-NULL
  setnames(temp,c("snow48h.orig","snow7d.orig"),c("snow48h","snow7d"))
  writeLines(toJSON(temp,dataframe = "rows",factor="string",na="null",pretty=TRUE),con = paste0("output/json/snowreports_",str_replace_all(string = x," ",replacement = "_"),"_07d.json"))
})

resorts<-as.list(unique(dt.snowdata$resort))
lapply(resorts,FUN = function(x){
  temp<-subset(dt.snowdata[resort==x & as.Date(time)>(as.Date(last.update)-30),],select=c(time,resort,snow12h,snow24h, snow48h.orig, snow7d.orig,base,cum))
  temp[,ReportDate:=as.Date(time),]
  temp$time<-NULL
  setnames(temp,c("snow48h.orig","snow7d.orig"),c("snow48h","snow7d"))
  writeLines(toJSON(temp,dataframe = "rows",factor="string",na="null",pretty=TRUE),con = paste0("output/json/snowreports_",str_replace_all(string = x," ",replacement = "_"),"_30d.json"))
})


dt.snowdata<-dt.snowdata[diff.hrs==min.diff.hrs & as.Date(time)>(as.Date(last.update)-7),]

dt.snowdata<-dt.snowdata[order(updated,decreasing = TRUE),]
setkeyv(dt.snowdata,c("resort","date"))
dt.snowdata<-unique(dt.snowdata)

# Check number of rows for each resort:
if(max(dt.snowdata[,list(NRows=mean(.N)),by=c("resort")]$NRows,na.rm=TRUE)>7){
  pbPost(type = "note",title = "Snow Table Error!",body = "More than 7 rows for a resort!")
} # should be 7 or less! 

dt.snowdata[,snow48h:=sum(ifelse(date>=max(date)-1,snow24h,NA),na.rm=TRUE),by=c("resort")]
dt.snowdata[,snow7d:=sum(ifelse(date>=max(date)-7,snow24h,NA),na.rm=TRUE),by=c("resort")]

dt.snowdata[,last.updated:=max(updated),by=c("resort")]

dt.snowtable<-subset(dt.snowdata[time==last.update | updated==last.updated,],select=c(time,resort,snow24h,snow48h,snow7d)) #& as.Date(time)==as.Date(updated)

dt.snowtable[,url:=paste0("http://snow-reports.ca/resorts/",str_replace_all(str_replace_all(resort,pattern = "\\.","")," ","-"),"/"),]
dt.snowtable[,resort.w.link:=paste0('<a href="',url,'" >',resort,'</a>'),]

dt.snowtable<-subset(dt.snowtable,select=c(resort,snow24h,snow48h,snow7d,time,url))

#setnames(dt.snowtable,c("Resort","24h\n(cm)","48h\n(cm)","7d\n(cm)","Updated","Link"))
dt.snowtable[,time:=as.character(ifelse(hour(time)>2,format(dt.snowtable$time,format="%b %d, %H:%M %Z"),format(dt.snowtable$time,format="%b %d"))),]

dt.snowtable<-data.table(merge(dt.snowtable,data.table(resortStatus),by=c("resort"),all.x = TRUE))
dt.snowtable[as.Date(time,format = "%b %d, %H:%M")!=as.Date(Sys.Date()),Status:=FALSE]
dt.snowtable.json<-copy(dt.snowtable)

dt.snowtable.json[Status==FALSE,snow24h:=NA,]
dt.snowtable.json[Status==FALSE,snow48h:=NA,]
dt.snowtable.json[Status==FALSE,snow7d:=NA,]

dt.snowtable.json<-dt.snowtable.json[order(-snow24h,resort,na.last = TRUE),]

dt.snowtable.json[,snow24h:=as.character(snow24h),]
dt.snowtable.json[,snow48h:=as.character(snow48h),]
dt.snowtable.json[,snow7d:=as.character(snow7d),]

dt.snowtable.json[Status==FALSE,snow24h:="-",]
dt.snowtable.json[Status==FALSE,snow48h:="-",]
dt.snowtable.json[Status==FALSE,snow7d:="-",]

# Get EC 24h Forecast
dt.forecastdata<-data.table(dbGetQuery(con,"SELECT DISTINCT resort,`temp.high`,`temp.low`,snow,rain,dmax FROM ecforecast24 WHERE dmax = (SELECT max(dmax) FROM ecforecast24)ORDER BY `id`"))
dt.forecastdata[resort=="Mt Washington",resort:="Mt. Washington",]
setnames(dt.forecastdata,c("snow","rain"),c("fcstSnow","fcstRain"))
dt.forecastdata[,fcstSnow:=round(fcstSnow,0),]

dt.snowtable.json<-merge(dt.snowtable.json,dt.forecastdata,by=c("resort"))

writeLines(toJSON(dt.snowtable.json,factor="string",na="null",pretty=TRUE),con = "output/json/snowtable.json")
dt.snowtable$Status<-NULL

#setnames(dt.snowtable,c("Resort","24h\n(cm)","48h\n(cm)","7d\n(cm)","Updated"),c("resort","snow24h","snow48h","snow7d","time"))

#### Disconnect from DB ####
on.exit(dbDisconnect(con))


#### Pushbullet (testing) ####
require(RPushbullet)

dt.snowtable<-dt.snowtable[order(snow24h,decreasing = TRUE),]
update.body<-paste(apply(X = dt.snowtable[order(snow24h,decreasing = TRUE),],MARGIN = 1,FUN = function(x){paste0(x['resort']," (",x['time'],")\n   24h: ",formatC(x['snow24h'],width=2,flag=" "),"cm",", 48h: ",formatC(x['snow48h'],width=2,flag=" "),"cm",", 7d: ",formatC(x['snow7d'],width=3,flag=" "),"cm")}),sep = "\n",collapse = "\n---\n")
#pbPost(type = "note",title = "Snow Update",body = update.body,recipients = "mike.birdgeneau@gmail.com")

dt.snowtable[parse_date_time(time,orders=c("md R"))>Sys.time()-(36*3600),]
update.body<-paste(apply(X = dt.snowtable[parse_date_time(time,orders=c("md R"))>Sys.time()-(36*3600),],MARGIN = 1,FUN = function(x){paste0(x['resort']," 24h: ",formatC(x['snow24h'],width=2,flag=" "),"cm")}),sep = "\n",collapse = "\n")
#pbPost(type = "note",title = paste0("Snow Reports for ",as.POSIXlt(Sys.time())),body = update.body,channel = "snowreportsca")
pushover(update.body)


#### Save copy to compare against later (avoid duplicate notifications in future):
dir.create(path = "cache/snowreports",showWarnings = FALSE)
dt.snowtable.copy<-copy(dt.snowtable)
save(dt.snowtable.copy,file="cache/dt.snowtable.copy.Rda")

#### Perform High Priority Notifications if Resorts are failing for more than 24h.
if(hour(Sys.time())>=18 & month(Sys.Date()) %in% c(12,1,2,3,4)){
  con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
  Sys.time()-24*3600
  dt.snowtable
  unique_resorts <- data.table(dbGetQuery(con,"SELECT resort,MAX(time) FROM snowdatav2 GROUP BY resort"))
  setnames(unique_resorts,c("resort","updated"))
  unique_resorts[,allowable_time:=Sys.time()-36*3600,]
  unique_resorts[resort=="Purden",allowable_time:=Sys.time()-7*3600,]
  notify_errors <- unique_resorts[updated<=allowable_time & resort!="Purden",]
  if(nrow(notify_errors)>0){
    pushover_high(message = paste("Resorts not updated in >36h: ",paste(notify_errors$resort,collapse = ", ")),title="Resort Snowfall Outdated (>36h)")
  } else {
    pushover("All resorts have snowfall reported in the past 36h.",title="Snow Reports Updated Successfully.")
  }
}

system("killall chromium-browser")
