# Generate new 7 & 30 day plots

# Load Required Packages
suppressPackageStartupMessages(require(RMySQL))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(stringr))

setwd("/home/snowreports/R/weather/")

# Connect to MySQL Database
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
# Get list of resorts
dt.snowdata<-data.table(dbReadTable(con,'snowdata'))
resortlist<-unique(dt.snowdata$resort)

# For each resort, get 7 & 30 day snowfall to plot.
for(i in 1:length(resortlist)){
  resort<-resortlist[i]
  cat(paste0("* Generating plots for ",resort,"...\n"))
  sql<-paste0("select * from snowdata where `resort` like '",resort,"' order by `time` desc limit 35;")
  dt.resort<-dbGetQuery(con,sql)
  today<-Sys.Date()
  dt.resort$time<-as.Date(dt.resort$time)
  dt.plot30<-subset(dt.resort,time>=(today-29),select=c(resort,time,snow24h))
  dt.plot7<-subset(dt.resort,time>=(today-6),select=c(resort,time,snow24h))
  days.30<-data.table(data.frame(time=seq(today-29, today, by=1)))
  days.7<-data.table(data.frame(time=seq(today-6, today, by=1)))
  if (nrow(dt.plot7)>1)
  {
    dt.plot30$time<-as.Date(dt.plot30$time)
    dt.plot7$time<-as.Date(dt.plot7$time)
  } else {
    next
  }
  dt.plot30<-merge(days.30,dt.plot30,by=c("time"),all.x=TRUE)
  dt.plot7<-merge(days.7,dt.plot7,by=c("time"),all.x=TRUE) 
 
  # Create 7 day plot
  file.loc=str_replace_all(paste0("snowcharts/7d_snowfall_",resort,".png")," ","_")
  png(filename=file.loc,width=542,height=208,units="px",res=72,type="cairo")
  p<-ggplot(data=dt.plot7,aes(x=time,y=snow24h))+geom_bar(stat="identity",fill="steelblue2",binwidth=1)+xlab("Date")+ylab("Snowfall (cm)")+
    geom_text(aes(label=snow24h), vjust=0,size=5)+
    opts(title=paste0(resort," 7 Day Snowfall History - Snow-Reports.ca"))+
    ylim(0,max(c(0,dt.plot7$snow24h),na.rm=TRUE)+2)
  print(p)
  dev.off()
  
  # Create 30 day plot
  file.loc=str_replace_all(paste0("snowcharts/30d_snowfall_",resort,".png")," ","_")
  png(filename=file.loc,width=542,height=208,units="px",res=72,type="cairo")
  p<-ggplot(data=dt.plot30,aes(x=time,y=snow24h))+geom_bar(stat="identity",fill="steelblue2",binwidth=1)+xlab("Date")+ylab("Snowfall (cm)")+
    geom_text(aes(label=snow24h), vjust=0,size=5)+
    opts(title=paste0(resort," 30 Day Snowfall History - Snow-Reports.ca"))+
    ylim(0,max(c(0,dt.plot30$snow24h),na.rm=TRUE)+2)
  print(p)
  dev.off()
  rm(dt.resort,dt.plot30,dt.plot7)
}

on.exit(dbDisconnect(con))
