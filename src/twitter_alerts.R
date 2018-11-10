library(ProjectTemplate)
suppressMessages(reload.project())

# setup_twitter_oauth() # Needs API Key etc

con<-dbConnect(MySQL(),user="snowreports",password="<db password>",dbname="snowfall",host="localhost")
dt.forecastdata<-data.table(dbGetQuery(con,"SELECT resort,`temp.high`,`temp.low`,snow,rain,dmax FROM ecforecast24 WHERE dmax = (SELECT max(dmax) FROM ecforecast24)ORDER BY `id`"))

dt.forecast48<-data.table(dbGetQuery(con,"SELECT resort,`temp.high`,`temp.low`,snow,rain,dmin FROM ecforecast48 WHERE dmax = (SELECT max(dmax) FROM ecforecast48)ORDER BY `id`"))

#dt.forecastdata<-data.table(dbGetQuery(con,"SELECT resort,`temp.high`,`temp.low`,snow,rain,dmax FROM ecforecast24 WHERE dmax = '2015-11-17 11:00:00' ORDER BY `id`"))
dbDisconnect(con)
rm(con)

dt.forecastdata<-merge(dt.local.cell.info,dt.forecastdata,by=c("resort"))
dt.forecastdata <- dt.forecastdata[mtn.range!="",]
forecast_summary<-dt.forecastdata[,list(snow.low=round(quantile(snow,na.rm=TRUE,probs=c(0.1)),0),snow.high=round(quantile(snow,na.rm=TRUE,probs=c(0.9)),0),temp.low=round(mean(temp.low,na.rm=TRUE),0),temp.high=round(mean(temp.high,na.rm=TRUE),0),rain.low=round(quantile(rain,na.rm=TRUE,probs=c(0.1)),0),rain.high=round(quantile(rain,na.rm=TRUE,probs=c(0.9)),0)),by=c("mtn.range")]

forecast_summary[,snow:=paste0(snow.low,"-",snow.high,"cm"),]
forecast_summary[abs(snow.low-snow.high)<5,snow:=paste0((round((snow.low+snow.high)/2,0)),"cm"),]

forecast_summary[,rain:=paste0(rain.low,"-",rain.high,"mm"),]
forecast_summary[abs(rain.low-rain.high)<5,rain:=paste0((round((rain.low+rain.high)/2,0)),"mm"),]

forecast_summary[,text:=paste0(mtn.range,": ",temp.low,"/",temp.high,"C, ",snow," snow",ifelse(rain.high>5,paste0(", ",rain," rain"),"")),]
forecast_summary[,text_snow:=paste0(mtn.range,": ",snow," snow",ifelse(rain.high>5,paste0(", ",rain," rain"),"")),]
forecast_summary[,text_rain:=paste0(mtn.range,": ",rain," rain",ifelse(snow.high>5,paste0(", ",snow," snow"),"")),]

forecast_summary[,text_cold:=paste0(mtn.range,": ",temp.low,"°C/",temp.high,"°C"),]



forecast_summary[,include:="",]
forecast_summary[temp.high>=5 | temp.low>0,include:="Warm",]
forecast_summary[temp.low<=-25 | temp.high < (-18), include:="Cold",]
forecast_summary[rain.high>=5,include:="Rain",]
forecast_summary[snow.high>10,include:="Snow",]

updateStatus(paste0("48hr Snow Forecast:\n",format(as.POSIXct(min(dt.forecast48$dmin),tz = "America/Edmonton"),format = "%a %b %d %Y %H:%M %Z"),"\nhttp://bit.ly/1Nd76fU"),displayCoords = FALSE,mediaPath = "graphs/48h_forecastGEM_snow.png")

updateStatus(paste0("24hr Snow Forecast:\n",format(as.POSIXct(max(dt.forecastdata$dmax),tz = "America/Edmonton"),format = "%a %b %d %Y %H:%M %Z"),"\nhttp://bit.ly/1lbRo81"),displayCoords = FALSE,mediaPath = "graphs/24h_forecast_snow.png")

if(nrow(forecast_summary[include=="Snow",])>0){
  forecast_summary<-forecast_summary[order(-snow.high),]
  msg<-"24h Fcst Snow:"
  i <- 1
  sentinel <- TRUE
  while(sentinel){
    if(i>nrow(forecast_summary[include=="Snow",])){sentinel <- FALSE} else {
      if(str_length(paste0(msg,forecast_summary[include=="Snow",]$text_snow[i]))<140){
        msg<-paste0(msg,"\n",forecast_summary[include=="Snow",]$text_snow[i])
        i <- i + 1
      } else {
        sentinel <- FALSE
      }
    }
  }
  updateStatus(msg, displayCoords = FALSE)
}

if(nrow(forecast_summary[include=="Cold",])>0){
  forecast_summary<-forecast_summary[order(temp.low),]
  msg<-"Cold Temps:"
  i <- 1
  sentinel <- TRUE
  while(sentinel){
    if(i>nrow(forecast_summary[include=="Cold",])){sentinel <- FALSE} else {
      if(str_length(paste0(msg,forecast_summary[include=="Cold",]$text_cold[i]))<140){
        msg<-paste0(msg,"\n",forecast_summary[include=="Cold",]$text_cold[i])
        i <- i + 1
      } else {
        sentinel <- FALSE
      }
    }
  }
  updateStatus(msg, displayCoords = FALSE)
}

if(nrow(forecast_summary[include=="Warm",])>0 & (month(Sys.Date())<=4 | month(Sys.Date()>=10))){
  forecast_summary<-forecast_summary[order(-temp.high),]
  msg<-"Warm Temps:"
  i <- 1
  sentinel <- TRUE
  while(sentinel){
    if(i>nrow(forecast_summary[include=="Warm",])){sentinel <- FALSE} else {
      if(str_length(paste0(msg,forecast_summary[include=="Warm",]$text_cold[i]))<140){
        msg<-paste0(msg,"\n",forecast_summary[include=="Warm",]$text_cold[i])
        i <- i + 1
      } else {
        sentinel <- FALSE
      }
    }
  }
  updateStatus(msg, displayCoords = FALSE)
}

if(nrow(forecast_summary[include=="Rain",])>0){
  forecast_summary<-forecast_summary[order(-temp.high),]
  msg<-"Fcst Rain:"
  i <- 1
  sentinel <- TRUE
  while(sentinel){
    if(i>nrow(forecast_summary[include=="Rain",])){sentinel <- FALSE} else {
      if(str_length(paste0(msg,forecast_summary[include=="Rain",]$text_rain[i]))<110){
        msg<-paste0(msg,"\n",forecast_summary[include=="Rain",]$text_rain[i])
        i <- i + 1
      } else {
        sentinel <- FALSE
      }
    }
  }
  msg <- paste0(msg,"\n","http://bit.ly/1SRgaVY")
  updateStatus(msg, displayCoords = FALSE, mediaPath = "graphs/24h_forecast_rain.png")
}

#updateStatus("24h Avg. Cloud Cover:",mediaPath = "graphs/24h_forecast_snow_animation.mp4")
