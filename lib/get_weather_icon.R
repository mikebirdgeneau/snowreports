logo.lookup<-expand.grid(daylight=c(TRUE,FALSE),cloudy=c(0,1,2),snow.qty=c(0,1,2),rain.qty=c(0,1,2))
logo.lookup$logoid<-c("01","02","03","07","04","05","14","34","14","34","24","24","15","35","15","35","25","25")

`%ni%` <- Negate(`%in%`)

get.weather.logo<-function(plot.data=plot.data,type="resorts")
{
  if(c("sunset") %ni% colnames(plot.data))
  {
  local.sun<-fetch.sunsriset(poi=plot.data$resort[1],type=type,dateTime=plot.data$date)
  plot.data$sunrise<-local.sun$sunrise
  plot.data$sunset<-local.sun$sunset
  }
  plot.data[,daylight:=ifelse(date>=(sunrise-90) & date<= (sunset+90),TRUE,FALSE),]
  
  plot.data[,cloudy:=ifelse(clouds<=0.25,0,ifelse(clouds>=0.75,2,1)),]
  plot.data[,snow.qty:=ifelse(snow.inst>0,ifelse(snow.inst>=3,2,1),0),]
  plot.data[,rain.qty:=ifelse(rain.inst>0,ifelse(rain.inst>=3,2,1),0),]
  
  logo.data<-subset(plot.data,select=c(date,daylight,cloudy,snow.qty,rain.qty))
  setkeyv(logo.data,c("date"))
  logo.result<-merge(logo.data,logo.lookup,by=c("daylight","cloudy","snow.qty","rain.qty"))
  setkeyv(logo.result,c("date"))
  logo.result[,logo.path:=paste0("simple_weather_icon_",logoid,".png"),]
  return(logo.result$logo.path)
}