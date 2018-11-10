require(RMySQL)
require(ggplot2)
require(gridExtra)
require(reshape)
require(stringr)
require(data.table)

df.swe<-read.csv(file="http://bcrfc.env.gov.bc.ca/data/asp/realtime/data/SW.csv",header=TRUE)
df.sd<-read.csv(file="http://bcrfc.env.gov.bc.ca/data/asp/realtime/data/SD.csv",header=TRUE)
df.cp<-read.csv(file="http://bcrfc.env.gov.bc.ca/data/asp/realtime/data/PC.csv",header=TRUE)
df.temp<-read.csv(file="http://bcrfc.env.gov.bc.ca/data/asp/realtime/data/TA.csv",header=TRUE)

df.swe<-data.table(melt(df.swe))
df.swe$type<-"Snow.Pillow"
df.swe$variable<-str_replace_all(df.swe$variable,".Snow.Pillow","")

df.sd<-data.table(melt(df.sd))
df.sd$type<-"Snow.Depth"
df.sd$variable<-str_replace_all(df.sd$variable,".Snow.Pillow","")

df.cp<-data.table(melt(df.cp))
df.cp$type<-"Cum.Precip"
df.cp$variable<-str_replace_all(df.cp$variable,".Snow.Pillow","")

df.temp<-data.table(melt(df.temp))
df.temp$type<-"Air.Temp.C"
df.temp$variable<-str_replace_all(df.temp$variable,".Snow.Pillow","")

# Combine tables.
bcsnowdata<-rbind(df.swe,df.temp,df.sd,df.cp)
setnames(bcsnowdata,c("DATE..UTC.","variable"),c("Date","Location"))
bcsnowdata<-data.frame(cast(bcsnowdata,Location+Date~type))
bcsnowdata<-subset(bcsnowdata,is.finite(Air.Temp.C) | is.finite(Snow.Depth) | is.finite(Snow.Pillow) | is.finite(Cum.Precip))
bcsnowdata$Date<-strptime(bcsnowdata$Date,format="%Y-%m-%d %H:%M:%S",tz="America/Edmonton")

con<-dbConnect(dbDriver("MySQL"), host="localhost",user="snowreports", pass="dbpass",db="basindata")

if(dbExistsTable(con, "bcbasindata")){
  dbWriteTable(con,"bcbasindata",bcsnowdata,append=T)
} else {
  dbWriteTable(con,"bcbasindata",bcsnowdata)
}

rm(bcsnowdata)


# FETCH DISTINCT DATA FROM DB

bc.basindata<-NULL
bc.basindata<-data.frame()

bc.basindata<-dbGetQuery(con,"SELECT DISTINCT Location,Date,Snow_Pillow,Snow_Depth,Air_Temp_C FROM bcbasindata ORDER BY Date DESC LIMIT 10000")
colnames(bc.basindata)<-c("Location","Date","Snow.Water.Equiv.mm","HS.cm","Air.Temp.C")
bc.basindata$Date<-as.POSIXct(bc.basindata$Date,format="%Y-%m-%d %H:%M:%S",tz="America/Edmonton")
bc.basindata<-subset(bc.basindata,Date>as.POSIXct((Sys.Date()-14)))
#bc.basindata<-merge(bc.basindata,subset(bc.locations,select=c(Location,names)),by=c("Location"))
str(bc.basindata)

loc<-unique(bc.basindata$Location)
length(loc)
rm(i)
for(i in 1:length(loc))
{
   cat(paste0("Generating plot ",i," (","graphs/", loc[i],".png",")\n"))
   plot.data<-subset(bc.basindata,loc[i]==Location)
   p.yrange<-range(plot.data$Snow.Water.Equiv.mm,na.rm=TRUE)
   p1.yrange<-range(plot.data$HS.cm,na.rm=TRUE)
   p2.yrange<-range(plot.data$Air.Temp.C,na.rm=TRUE)

   if(!is.finite(p.yrange[1])){p.yrange<-c(0,500)}
   if(!is.finite(p1.yrange[1])){p1.yrange<-c(0,500)}
   if(!is.finite(p2.yrange[1])){p2.yrange<-c(-15,15)}

   plot.data$names<-str_replace_all(substr(unique(plot.data$Location),start=8,stop=55),"\\."," ")
   plot.data$Date<-strptime(plot.data$Date,format="%Y-%m-%d %H:%M:%S")
      lastdate<-max(plot.data$Date)

   p<-ggplot(data=plot.data)+
      geom_point(aes(x=Date,y=Snow.Water.Equiv.mm),color="darkgrey")+
      geom_line(aes(x=Date,y=Snow.Water.Equiv.mm))+
      #geom_line(aes(x=Date,y=HS.cm),col="darkblue")+
      xlab("")+
      ylab("Snow Water Equiv. (mm)")+
      ylim(p.yrange)+
      ggtitle(paste0("Snowpack Data for ", unique(plot.data$names)," - Updated ", lastdate, " UTC"))
   p1<-ggplot(data=plot.data)+
      geom_point(aes(x=Date,y=HS.cm),color="darkgrey")+
      geom_line(aes(x=Date,y=HS.cm))+
      ylim(p1.yrange)+
      xlab("")+
      ylab("Snow Depth (cm)")
   p2<-ggplot(data=plot.data)+
      geom_point(aes(x=Date,y=Air.Temp.C),color="darkgrey")+
      geom_line(aes(x=Date,y=Air.Temp.C))+
      ylim(p2.yrange)+
      xlab("Date\nSnow-Reports.ca - Data: Province of British Columbia (http://www.data.gov.bc.ca/)")+
      ylab("Air Temperature (C)")
   png(filename=paste0("output/basins/", loc[i],".png"),type="cairo", width=640, height=640,units="px",res=72)
   grid.arrange(p,p1,p2,nrow=3,heights=c(0.35,0.35,0.3))
   dev.off()
}

rm(con)

