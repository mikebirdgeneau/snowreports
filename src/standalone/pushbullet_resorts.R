library(RMySQL)
library(RPushbullet)
library(data.table)
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
max.date<-dbGetQuery(con,"select MAX(dmax) FROM ecforecast24")[1,1]
max.dmin<-dbGetQuery(con,"select MAX(dmin) FROM ecforecast24")[1,1]


# Main Forecast Plots
#pbPost(type = 'file',title = "Cloud Cover",body = paste0("Cloud Cover for ",format(as.POSIXct(max.dmin),"%a %b %e")), url = "graphs/24h_forecast_cloud.png",filetype = "image/png",channel="snowreportsca")
#pbPost(type = 'file',body = paste0("Rain Forecast for ",format(as.POSIXct(max.dmin),"%a %b %e")), url = "graphs/24h_forecast_rain.png",filetype = "image/png",channel="snowreportsca")
#pbPost(type = 'file',body = paste0("Snow Forecast for ",format(as.POSIXct(max.dmin),"%a %b %e")), url = "graphs/24h_forecast_snow.png",filetype = "image/png",channel="snowreportsca")

pbPost(type = 'file',title = paste0("Snow Forecast for ",format(as.POSIXct(max.dmin),"%a %b %e")), body=paste0("Snow Forecast Update for ",format(as.POSIXct(max.dmin),"%a %b %e"), " \nhttp://snow-reports.ca/weather/24h-high-resolution-ec-hrdps-west/snow/day-1-0-24h"), url="graphs/24h_forecast_snow.png",channel="snowreportsca")


fcst.24h<-data.table(dbGetQuery(con,paste0("select * FROM ecforecast24 where dmax='",max.date,"'")))
fcst.48h<-data.table(dbGetQuery(con,paste0("select * FROM ecforecast48 where dmax='",max.date,"'")))
fcst.24h$id<-NULL
fcst.48h$id<-NULL

# Alert to forecast snowfall >10cm:
if(nrow(fcst.24h[snow>=10,])>0){
  push.data<-fcst.24h[snow>=10,]
  push.data<-push.data[order(snow,decreasing = TRUE),]
  pbPost(type="note",title = "24h Forecast Snowfall > 10cm for:",body = 
           paste0("Period starting ",format(as.POSIXct(min(push.data$dmin)),"%a %b %e %H:%m %z"),":\n",
                  paste(
                    apply(push.data,MARGIN = 1,function(x){
                      paste0(" * ",x['resort'],": ",round(as.numeric(x['snow']),0),"cm, ",round(as.numeric(x['temp.high']),0),"ºC / ",round(as.numeric(x['temp.low']),0),"ºC",ifelse(as.numeric(x['rain'])>0,paste0(", rain: ",round(as.numeric(x['rain']),0),"mm"),""),"")
                    }),collapse="\n")
           ),
         channel="snowreportsca")
}


update<-fcst.48h[resort=="Lake Louise" & fcst.day==1,]
update2<-fcst.48h[resort=="Lake Louise" & fcst.day==2,]
pbPost(type="note",title=paste0(update$resort," Forecast: ",format(as.POSIXct(update$dmin),"%a %b %e")),body=paste0("Today:\n ",round(update$temp.low,0),"C / ",round(update$temp.high),"C, ",round(update$snow,1),"cm / ",update$rain,"mm, Cloud: ",round(update$cloud.avg,2),"\n\n","Tomorrow:\n ",round(update2$temp.low,0),"C / ",round(update2$temp.high),"C, ",round(update2$snow,1),"cm / ",update2$rain,"mm, Cloud: ",round(update2$cloud.avg,2)),,channel="snowreportsca")#,local.url = "graphs/24h_forecast_Lake_Louise.png",file.type = "image/png")


update<-fcst.48h[resort=="Calgary NW" & fcst.day==1,]
update2<-fcst.48h[resort=="Calgary NW" & fcst.day==2,]
pbPost(type="note",title=paste0(update$resort," Forecast: ",format(as.POSIXct(update$dmin),"%a %b %e")),body=paste0("Today:\n ",round(update$temp.low,0),"C / ",round(update$temp.high),"C, ",round(update$snow,1),"cm / ",update$rain,"mm, Cloud: ",round(update$cloud.avg,2),"\n\n","Tomorrow:\n ",round(update2$temp.low,0),"C / ",round(update2$temp.high),"C, ",round(update2$snow,1),"cm / ",update2$rain,"mm, Cloud: ",round(update2$cloud.avg,2)),,recipients="mike.birdgeneau@gmail.com")#,local.url = "graphs/24h_forecast_Lake_Louise.png",file.type = "image/png")

#update<-unique(fcst.24h[resort=="Canmore",])
#pbPost(type="note",title=paste0(update$resort," HRDPS ",format(as.POSIXct(update$dmin),"%a %b %e")),body=paste0("Today: Low ",round(update$temp.low,0),"C, High ",round(update$temp.high),"C, Snow: ",round(update$snow,1),"cm, Rain: ",update$rain,"mm, Cloud: ",round(update$cloud.avg*100,0)," percent"))#,local.url = "graphs/24h_forecast_Lake_Louise.png",file.type = "image/png")

#update<-unique(fcst.24h[resort=="Revelstoke",])
#pbPost(type="note",title=paste0(update$resort," HRDPS ",format(as.POSIXct(update$dmin),"%a %b %e")),body=paste0("Today: Low ",round(update$temp.low,0),"C, High ",round(update$temp.high),"C, Snow: ",round(update$snow,1),"cm, Rain: ",update$rain,"mm, Cloud: ",round(update$cloud.avg*100,0)," percent"))#,local.url = "graphs/24h_forecast_Revelstoke.png",file.type = "image/png")

dbDisconnect(con)
rm(con,fcst.24h,fcst.48h)
