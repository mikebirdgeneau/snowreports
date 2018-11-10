require(RMySQL)
require(data.table)
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")

max.date<-dbGetQuery(con,"select MAX(dmax) FROM ecforecast24")[1,1]
fcst.24h<-data.table(dbGetQuery(con,paste0("select * FROM ecforecast24 where dmax='",max.date,"'")))
fcst.48h<-data.table(dbGetQuery(con,paste0("select * FROM ecforecast48 where dmax='",max.date,"'")))
fcst.24h$id<-NULL
fcst.48h$id<-NULL


source("lib/pushoverr.R")

update<-unique(fcst.24h[resort=="Lake Louise",])
pushover(message=paste0("Today: Low ",round(update$temp.low,0),"C, High ",round(update$temp.high),"C, Snow: ",round(update$snow,1),"cm, Rain: ",update$rain,"mm, Cloud: ",round(update$cloud.avg*100,0),"%"),title=paste0(update$resort," HRDPS ",format(as.POSIXct(update$dmin),"%a %b %e")))
update<-fcst.48h[resort=="Lake Louise" & fcst.day==1,]
update2<-fcst.48h[resort=="Lake Louise" & fcst.day==2,]
pushover(title=paste0(update$resort," RDPS ",format(as.POSIXct(update$dmin),"%a %b %e")),message=paste0("Today:\n Low ",round(update$temp.low,0),"C, High ",round(update$temp.high),"C,\n Snow: ",round(update$snow,1),"cm, Rain: ",update$rain,"mm,\n Cloud: ",round(update$cloud.avg*100,0),"%. \nTomorrow: Low ",round(update2$temp.low,0),"C, High ",round(update2$temp.high),"C, \n Snow: ",round(update2$snow,1),"cm, Rain: ",update2$rain,"mm,\n Cloud: ",round(update2$cloud.avg*100,0),"%"))


update<-unique(fcst.24h[resort=="Calgary",])
pushover(paste0(update$resort," ",format(as.POSIXct(update$dmin),"%a %b %e"),": Low ",round(update$temp.low,0),"C, High ",round(update$temp.high),"C, Snow: ",round(update$snow,1),"cm, Rain: ",update$rain,"mm, Cloud: ",round(update$cloud.avg*100,0),"%"))

update<-unique(fcst.24h[resort=="Canmore",])
pushover(paste0(update$resort," ",format(as.POSIXct(update$dmin),"%a %b %e"),": Low ",round(update$temp.low,0),"C, High ",round(update$temp.high),"C, Snow: ",round(update$snow,1),"cm, Rain: ",update$rain,"mm, Cloud: ",round(update$cloud.avg*100,0),"%"))

update<-unique(fcst.24h[resort=="Revelstoke",])
pushover(paste0(update$resort," ",format(as.POSIXct(update$dmin),"%a %b %e"),": Low ",round(update$temp.low,0),"C, High ",round(update$temp.high),"C, Snow: ",round(update$snow,1),"cm, Rain: ",update$rain,"mm, Cloud: ",round(update$cloud.avg*100,0),"%"))

dbDisconnect(con)
rm(con,fcst.24h,fcst.48h)