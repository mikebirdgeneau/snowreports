require(RMySQL)
require(ggplot2)
require(gridExtra)
library(data.table)

options(stringsAsFactors = FALSE)

# List of Alberta Government Basin Snow Depth Locations

ab.locations<-data.table(Location=c("05BB803","05CA805","05BF824","05BJ805","05BL812","05DD804","05AA809","05AD803"),
			names=c("Sunshine Village","Skoki Lodge","Three Isle Lake","Little Elbow Summit","Mount Odium","Southesk","Gardiner Creek","Akamina Pass"),
                         urls=c("http://www.environment.alberta.ca/apps/Basins/data/text/snow/05BB803.csv",
                                "http://www.environment.alberta.ca/apps/Basins/data/text/snow/05CA805.csv",
                                "http://www.environment.alberta.ca/apps/Basins/data/text/snow/05BF824.csv",
                         "http://www.environment.alberta.ca/apps/Basins/data/text/snow/05BJ805.csv",
                         "http://www.environment.alberta.ca/apps/Basins/data/text/snow/05BL812.csv",
                         "http://www.environment.alberta.ca/apps/Basins/data/text/snow/05DD804.csv",
			 "http://www.environment.alberta.ca/apps/Basins/data/text/snow/05AA809.csv",
			 "http://www.environment.alberta.ca/apps/Basins/data/text/snow/05AD803.csv"))

# FETCH DATA and UPDATE DB:

ab.basindata<-NULL
ab.basindata<-data.table()

for(i in 1:nrow(ab.locations)){
  temp<-data.frame(read.csv(as.character(ab.locations$urls[i]),sep=",",header=TRUE,skip=21))
  if(i==1){
    ab.basindata<-temp
  } else {
    ab.basindata<-rbind(ab.basindata,temp)
  }
}


colnames(ab.basindata)<-c("Location","Date","Snow.Water.Equiv.mm","HS.cm","Air.Temp.C")
ab.basindata<-subset(ab.basindata,Location!="** End of report **")
ab.basindata$Date<-strptime(ab.basindata$Date,format="%Y-%m-%d %H:%M:%S",tz="America/Edmonton")

con<-dbConnect(dbDriver("MySQL"), host="localhost",user="snowreports", pass="dbpass",db="basindata")

if(dbExistsTable(con, "abbasindata")){
  dbWriteTable(con,"abbasindata",ab.basindata,append=T)
} else {
  dbWriteTable(con,"abbasindata",ab.basindata)
}

rm(ab.basindata)

# FETCH DISTINCT DATA FROM DB

ab.basindata<-NULL
ab.basindata<-data.table()

ab.basindata<-dbGetQuery(con,"SELECT DISTINCT Location,Date,Snow_Water_Equiv_mm,HS_cm,Air_Temp_C FROM abbasindata ORDER BY Date DESC LIMIT 1000")
colnames(ab.basindata)<-c("Location","Date","Snow.Water.Equiv.mm","HS.cm","Air.Temp.C")
ab.basindata$Date<-as.POSIXct(ab.basindata$Date,format="%Y-%m-%d %H:%M:%S",tz="America/Edmonton")
ab.basindata<-subset(ab.basindata,Date>=as.POSIXct(Sys.Date()-14))
ab.basindata<-merge(ab.basindata,subset(ab.locations,select=c(Location,names)),by=c("Location"))
ab.basindata<-data.table(ab.basindata)

loc<-unique(ab.basindata$Location)
length(loc)
rm(i)
for(i in 1:length(loc))
{
   cat(paste0("Generating plot ",i," (","output/basins/",tolower(str_replace_all(string = unique(ab.basindata[Location==loc[i],names,])," ","_")),".png",")\n"))
   plot.data<-subset(ab.basindata,loc[i]==Location)
   lastdate<-max(plot.data$Date)
   p<-ggplot(data=plot.data)+
      geom_point(aes(x=Date,y=Snow.Water.Equiv.mm),color="darkgrey")+
      geom_line(aes(x=Date,y=Snow.Water.Equiv.mm))+
      xlab("")+
      #xlab("Date\nSnow-Reports.ca - Data: Government of Alberta (http://www.environment.alberta.ca/)")+
      ylab("Snow Water Equiv. (mm)")+
      ggtitle(paste0("Snowpack Data for ", unique(plot.data$names)," - Updated ", lastdate))
   if(sum(ifelse(is.finite(plot.data$Air.Temp.C),1,0))==0){plot.data$Air.Temp.C[1]<-0}
   p2<-ggplot(data=plot.data)+
      geom_point(aes(x=Date,y=Air.Temp.C),color="darkgrey")+
      geom_line(aes(x=Date,y=Air.Temp.C))+
      xlab("Date\nSnow-Reports.ca - Data: Government of Alberta (http://www.environment.alberta.ca/)")+
      ylab("Air Temperature (C)")+
   png(filename=paste0("output/basins/", tolower(str_replace_all(string = unique(ab.basindata[Location==loc[i],names,])," ","_")),".png"),type="cairo", width=640, height=480,units="px",res=72)
   grid.arrange(p,p2,nrow=2,heights=c(0.6,0.4))
   dev.off()
}

rm(con)

