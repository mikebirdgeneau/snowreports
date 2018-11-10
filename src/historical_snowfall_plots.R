require(RMySQL)
require(ggplot2)
require(gridExtra)
require(lubridate)

# Connect to Database
con<-dbConnect(dbDriver("MySQL"), host="localhost",user="snowreports", pass="dbpass",db="snowfall")

# Create Data to Plot

plot.data<-data.table(dbGetQuery(con,"SELECT DISTINCT resort,time,snow24h FROM snowdata"))
plot.data[,date:=as.Date(time),]
setkeyv(plot.data,c("resort","date"))
plot.data<-unique(plot.data)
plot.data[,year:=year(time),]
plot.data[,month:=month(time),]
#plot.data<-plot.data[,list(monthly.snow.cm=sum(snow24h,na.rm=TRUE)),by=c("resort","year","month")]
#plot.data[,date:=as.Date(paste0(year,"/",month,"/1")),]
plot.data[,season:=ifelse(month>6,paste0(year,"-",year-1999),paste0(year-1,"-",year-2000)),]
plot.data[,day.of.year:=yday(as.POSIXct(time)+183*24*3600),]
setkeyv(plot.data,c("resort","season","date"))
plot.data[,cum.snow:=cumsum(snow24h),by=c("resort","season")]
#plot.data[,season.mo:=factor(month,levels=c(7,8,9,10,11,12,1,2,3,4,5,6),labels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun"),ordered=TRUE),]

# Plot Data
for(i in 1:length(unique(plot.data$resort)))
{
  png(filename=paste0("graphs/historical_snowfall_",str_replace_all(str_replace_all(unique(plot.data$resort)[i],"\\.","")," ","_"),".png"),width=800,height=350)
  print(ggplot(data=plot.data[resort==unique(plot.data$resort)[i],],aes(x=day.of.year,y=cum.snow,fill=as.factor(season),group=season))+geom_line(aes(colour=season))+xlab("Days since July 1")+ylab("Monthly Reported Snowfall (cm)")+scale_fill_discrete(name="Season")+ggtitle(paste0("Historical Reported Snowfall for ",unique(plot.data$resort)[i])))
  dev.off()
  
}

dbDisconnect(con)
rm(con,plot.data)
