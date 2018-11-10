# Required Packages
#install.packages(c("ncdf","mapproj","fields","sp","rgdal","maptools","raster","gstat"))
require(ncdf)
require(mapproj)
require(sp)
require(rgdal)
require(gstat)
require(stringr)
require(data.table)
require(ggplot2)
require(RColorBrewer)
library(gridExtra)
require(RMySQL)
require(reshape)
require(plotKML)


#setwd("/home/snowreports/R/weather/")
Sys.setenv(TZ='America/Edmonton')
cur.tz<-format(Sys.time(),"%Z")
Sys.setenv(TZ=cur.tz)

# Get current date in UTC
date=format(as.POSIXct(Sys.time()-7*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")

model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
#dims=dim(ncvar_get(model0,"LAND_surface"))
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")

# Add annotations for resorts
#source("lib/resorts.R")

#rownames(resorts)<-c("Resort","x","y")
#colnames(resorts)<-NULL
#resorts<-data.frame(t(resorts))
#resorts$x=as.numeric(as.character(resorts$x))
#resorts$y=as.numeric(as.character(resorts$y))
#write.csv(resorts,file="data/resorts.csv",row.names=FALSE)
#coordinates(resorts)<-c('x','y')
#proj4string(resorts)='+proj=longlat +ellps=GRS80 +towgs84'
resorts<-poi.resorts

# Set-up Grid Cells
x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
coords=data.frame(x=x,y=y)
coordinates(coords)=~x+y
proj4string(coords)<-CRS("+proj=latlong")
grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(2500,2500),c(685,485)))
rm(x,y)
# Find grid cells closest to resorts.
resort.cells<-data.frame(resort=resorts@data$Resort,x=resorts@coords[,1],y=resorts@coords[,2])
resort.cells$cellno<-NA
rm(resorts)
for(i in 1:nrow(resort.cells)){
  resort.cells$cellno[i]<-which.min(sqrt(abs(coords@coords[,1]-resort.cells$x[i])^2+abs(coords@coords[,2]-resort.cells$y[i])^2))
}
resort.cells$cell.x<-coords@coords[resort.cells$cellno,1]
resort.cells$cell.y<-coords@coords[resort.cells$cellno,2]

#Convert to Row / Column Index
resort.cells$row<-ceiling(resort.cells$cellno/685)
resort.cells$col<-685-(resort.cells$row*685-resort.cells$cellno)
resort.cells

# Get data by resort, over 24 hr period.
plot.data<-data.frame()
for(i in 1:nrow(resort.cells)){
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1),count=c(24)),tz="UTC",origin="1970-01-01"),tz=cur.tz))
  tmp.lat<-resort.cells$y[i]
  tmp.long<-resort.cells$x[i]
  #tmp.height<-ncvar_get(model0,"HGT_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,1))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))-273.15
  tmp.sprate<-ncvar_get(model,"SPRATE_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.rprate<-ncvar_get(model,"RPRATE_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.clouds<-ncvar_get(model,"TCDC_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))/100
  tmp.wind<-ncvar_get(model,"WIND_10maboveground",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))*3.6
  tmp.winddir<-ncvar_get(model,"WDIR_10maboveground",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.press<-ncvar_get(model,"PRES_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))/1E3
  # Get elevation pressure profiles
  tmp.hgt.800mb<-ncvar_get(model,"HGT_800mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.hgt.900mb<-ncvar_get(model,"HGT_900mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.hgt.1000mb<-ncvar_get(model,"HGT_1000mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.tmp.800mb<-ncvar_get(model,"TMP_800mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))-273.15
  tmp.tmp.900mb<-ncvar_get(model,"TMP_900mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))-273.15
  tmp.tmp.1000mb<-ncvar_get(model,"TMP_1000mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))-273.15
  tmp.wdir.800mb<-ncvar_get(model,"WDIR_800mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.wdir.900mb<-ncvar_get(model,"WDIR_900mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.wdir.1000mb<-ncvar_get(model,"WDIR_1000mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.wind.800mb<-ncvar_get(model,"WIND_800mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.wind.900mb<-ncvar_get(model,"WIND_900mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.wind.1000mb<-ncvar_get(model,"WIND_1000mb",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  
  tmp<-data.frame(
    resort=resort.cells$resort[i],
    date=tmp.date,
    lat=tmp.lat,
    long=tmp.long,
   #surf.elev=tmp.height,
    temperature=tmp.temperature,
    snow=tmp.sprate,
    rain=tmp.rprate,
    apcp=tmp.apcp,
    clouds=tmp.clouds,
    wind=tmp.wind,
    winddir=tmp.winddir,
    press=tmp.press)
  rm(tmp.apcp,tmp.clouds,tmp.date,tmp.press,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir)
  ifelse(i==1,plot.data<-tmp,plot.data<-rbind(plot.data,tmp))
}

# Load Data
load("output/kml_data.Rda")

# Set-up function for directions
getDir<-function(deg)
{
  dirs<-c(rep("N",times=23),rep("NE",times=45),rep("E",times=45),rep("SE",times=45),
          rep("S",times=45),rep("SW",times=45),rep("W",times=45),rep("NW",times=45),
          rep("N",times=23))
  return(dirs[round(deg,0)])
}

plot.kml<-data.table(plot.data)
plot.kml<-plot.kml[,list(
  date=min(date),
  lat=mean(lat),
  long=mean(long),
  low.temp=round(min(temperature),0),
  max.temp=round(max(temperature),0),
  snow=round(max(snow),1),
  rain=round(max(rain),1),
  apcp=round(max(apcp),1),
  clouds=paste0(round(mean(clouds),2)*100,"%"),
  wind=round(mean(wind),2),
  winddir=round(median(winddir),0),
  press=paste0(round(mean(press),0),"kPa")
),by=c("resort")]

coordinates(plot.kml)=~long+lat
proj4string(plot.kml)<-CRS('+proj=longlat')
plot.kml
plotkml<-spTransform(plot.kml,CRS('+proj=longlat +datum=WGS84'))

snowpred <- test2[3]
snowpred@data$snow.acc[which(snowpred@data$snow.acc<=0.999)]<-NA
proj4string(snowpred)<-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs')

plotkml@data$date<-format(as.POSIXct(plotkml@data$date-3600, tz="America/Edmonton"),"%Y-%m-%d %H:%M",tz="America/Edmonton")

# Generate HTML table for each resort
plotkml@data$htmltable<-NA
for(i in 1:nrow(plotkml))
{
  plotkml@data$htmltable[i]=paste0(
    "<h2>",plotkml@data$resort[i],"</h2><table>
    <tr><td>Date:</td><td>",plotkml@data$date[i],"</td></tr>
    <tr><td>Low:</td><td>",plotkml@data$low.temp[i],"&deg; C</td></tr>
    <tr><td>High:</td><td>",plotkml@data$max.temp[i],"&deg; C</td></tr>
    <tr><td>Forecast Snow:</td><td>",plotkml@data$snow[i]," mm H2O equiv.</td></tr>
    <tr><td>Forecast Rain:</td><td>",plotkml@data$rain[i]," mm </td></tr>
    <tr><td>Cloud Cover:</td><td>",plotkml@data$clouds[i],"</td></tr>
    <tr><td>Pressure:</td><td>",plotkml@data$press[i],"</td></tr>
    <tr><td>Wind:</td><td>",plotkml@data$wind[i]," ",getDir(plotkml@data$winddir[i]),"</td></tr>
    <tr><td colspan=2><a href='http://snow-reports.ca/weather/location/",str_replace_all(plotkml@data$resort[i]," ","_"),"/'>Details...</a></td></tr></table>")
}

# Set-up Colour Palette for Snow
snowPalette<-colorRampPalette(c("#FFFFFF", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142"))(1000)
snowColours<-snowPalette[round((seq(20,80,by=1))^2/6.4,0)]
snowColours[1]<-"#FFFFFF00"

plotKML.env(silent=TRUE)
kml_open(file.name="output/snowreports.kml",overwrite=TRUE,folder.name="Snow-Reports.ca")
kml_layer(plotkml,points_names=plotkml@data$resort,html.table=plotkml@data$htmltable,shape="http://maps.google.com/mapfiles/kml/shapes/snowflake_simple.png",size=0.7,colour="white",folder.name="Resorts")
kml_layer.SpatialPixels(snowpred,colour=snow.acc,plot.legend=TRUE,colour_scale=snowColours,balloon=FALSE,folder.name="Snowfall",alpha=1)
kml_close("output/snowreports.kml")
system("mv snow.png output/snow.png")
system("mv snow_legend.png output/snow_legend.png")
kml_compress(file.name="output/snowreports.kml")

gc()
stop("Testing KML Export")

plot.data<-data.table(plot.data)
setkeyv(plot.data,c("resort","date"))
#plot.data[,hrs:=(date-min(date))/3600+1,by=c("resort")]
plot.data[,snow.acc:=cummax(snow),by=c("resort")]
plot.data[,rain.acc:=cummax(rain),by=c("resort")]
rm(coords,grid,i,model,model0,resort.cells,tmp)#resort.cells.x,resort.cells.y)
gc()

# Generate Plots
resortlist<-unique(plot.data$resort)
for(i in 1:length(resortlist))
{
  resort.data<-subset(plot.data,resort==resortlist[i])
  resort.data$date<-as.POSIXct(resort.data$date,tz=cur.tz)
  resort.data$hour<-hour(resort.data$date)
  resort.data$daylight<-abs(resort.data$hour-12)^2
  resort.data$quad<-c(rep(c(1),times=6),rep(c(2),times=6),rep(c(3),times=6),rep(c(4),times=6))
  rects<-data.frame(xstart=resort.data$date[-24],xend=resort.data$date[-1],col=resort.data$daylight[-24]/11)
  rects$xstart[1]<-rects$xstart[1]-3900
  rects$xend[23]<-rects$xend[23]+3900
  resort.data$winddir.rad<-resort.data$winddir*pi/180
  resort.data$windfrac<-resort.data$wind/max(resort.data$wind)
  pcp.data<-subset(resort.data,select=c(resort,date,snow.acc,rain.acc))
  setnames(pcp.data,c("snow.acc","rain.acc"),c("Cum. Snow","Cum. Rain"))
  pcp.data<-melt(pcp.data,id.vars=c("resort","date"))
  p1<-ggplot()+
    geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
    scale_alpha_continuous(range=c(0,0.25),guide="none")+
    geom_bar(data=pcp.data,stat="identity",aes(x=date,y=value,fill=variable),position="stack")+
    #opts(title=paste0("Forecast Snowfall Accumulation at ",resort.data$resort[1]))+
    xlab("")+ylab("Accumulation (cm snow, mm rain)\n(cm @ 100kg/m3)")+
    scale_fill_manual(name="Legend",values = c("Cum. Snow" = "steelblue2","Cum. Rain" = "lightblue"))+
    #xlim(min(resort.data$date),max(resort.data$date))+
    ylim(0,max(5,resort.data$snow.acc,resort.data$rain.acc,resort.data$snow.acc+resort.data$rain.acc))
  
  p2<-ggplot()+
    geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
    scale_alpha_continuous(range=c(0,0.25),guide="none")+
    geom_line(data=resort.data,aes(x=date,y=temperature))+
    xlab(paste0("Forecast Time (",format(Sys.time(),"%Z",tz="America/Edmonton"),") - Source: Environment Canada"))+ylab("Temperature \n(Celsius)")+
    ylim(min(-10,resort.data$temperature),max(0,resort.data$temperature))+
    #xlim(min(resort.data$date),max(resort.data$date))+
    geom_hline(y=0,aes(colour="Freezing"),colour="Red")+
    scale_color_gradientn(name="Cloud Cover (frac)",colours=c("gold1","khaki","ivory3","ivory4"), limits=c(0,1),guide="colourbar")+
    geom_point(data=resort.data,aes(x=date,y=temperature,colour=clouds),size=4)
  
  wind.data<-subset(resort.data, quad == 1)
  wind.data<-wind.data[complete.cases(wind.data),]
  wind.pmax=max(resort.data$wind) #35
  p3<-ggplot(wind.data,aes(x=winddir,y=wind))+
    geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
    #geom_area(fill="dodgerblue")+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+ #labels=NULL
    opts(panel.background=theme_rect(fill = "white", colour = NA),
         panel.grid.major=theme_line(colour="darkgrey",size=0.5),
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("Wind")
  
  wind.data<-subset(resort.data, quad == 2)
  wind.data<-wind.data[complete.cases(wind.data),]
  p4<-ggplot(wind.data,aes(x=winddir,y=wind))+
    geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    opts(panel.background=theme_rect(fill = "white", colour = NA),
         panel.grid.major=theme_line(colour="darkgrey",size=0.5),
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("")
  
  wind.data<-subset(resort.data, quad == 3)
  wind.data<-wind.data[complete.cases(wind.data),]
  p5<-ggplot(wind.data,aes(x=winddir,y=wind))+
    geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    opts(panel.background=theme_rect(fill = "white", colour = NA),
         panel.grid.major=theme_line(colour="darkgrey",size=0.5),
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("")
  
  wind.data<-subset(resort.data, quad == 4)
  wind.data<-wind.data[complete.cases(wind.data),]
  p6<-ggplot(wind.data,aes(x=winddir,y=wind))+
    geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    opts(panel.background=theme_rect(fill = "white", colour = NA),
         panel.grid.major=theme_line(colour="darkgrey",size=0.5),
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("")
  
  #elev<-unique(resort.data$surf.elev)
  #cat(paste0("* Generating plot for ",resortlist[i]," (",elev,"m)","...\n"))
  cat(paste0("* Generating plot for ",resortlist[i],"...\n"))
  file.loc=str_replace_all(paste0("graphs/24h_forecast_",resortlist[i],"_tmp.png")," ","_")
  png(filename=file.loc,width=790,height=700,units="px",res=72,type="cairo")
  #png(filename=file.loc,width=790,height=700,units="px",res=72) # Run on local machine
  #grid.arrange(p1,p2,arrangeGrob(p3,p4,p5,p6, widths=c(1/4, 1/4, 1/4, 1/4), nrow=1), nrow=3,main = paste0("\nWeather Forecast for 24h Period at ",resort.data$resort[1]," (",elev,"m) - Snow-Reports.ca"))
  grid.arrange(p1,p2,arrangeGrob(p3,p4,p5,p6, widths=c(1/4, 1/4, 1/4, 1/4), nrow=1), nrow=3,main = paste0("\nWeather Forecast for 24h Period at ",resort.data$resort[1]," - Snow-Reports.ca"))
  dev.off()
}

gc()

for(i in 1:length(resortlist))
{
  file.loc=str_replace_all(paste0("graphs/24h_forecast_",resortlist[i],"_tmp.png")," ","_")
  file.dest=str_replace_all(paste0("graphs/24h_forecast_",resortlist[i],".png")," ","_")
  system(paste0("mv ",file.loc," ",file.dest,"\n"))
}

# Generate summary table for storing in database
daily.data<-plot.data[,list(min.date=min(date),max.date=max(date),temp.low=min(temperature),temp.high=max(temperature),snow=max(snow),rain=max(rain),pcp=max(apcp),cloud.avg=mean(clouds),wind.avg=mean(wind),wind.max=max(wind),wind.dir=mean(winddir),press=mean(press),snow.acc=max(snow.acc)),by="resort"]
filedate=format(min(daily.data$min.date),"%Y%m%d-%H",tz="America/Edmonton")
#write.csv(daily.data,file=paste0("output/",filedate,"-daily_data.csv"),na="",row.names=FALSE)

gc()
# Write rows to DB
con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
for(i in 1:nrow(daily.data)){
  sql<-sprintf("insert into ecforecast24 (id,resort,`dmin`,`dmax`,`temp.low`,`temp.high`,snow,rain,pcp,`cloud.avg`,`wind.avg`,`wind.max`,`wind.dir`,pressure,`snow.acc`) values (null,'%s','%s','%s',%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f);",daily.data$resort[i],daily.data$min.date[i],daily.data$max.date[i],daily.data$temp.low[i],daily.data$temp.high[i],daily.data$snow[i],daily.data$rain[i],daily.data$pcp[i],daily.data$cloud.avg[i],daily.data$wind.avg[i],daily.data$wind.max[i],daily.data$wind.dir[i],daily.data$press[i],daily.data$snow.acc[i]) 
  rs<-dbSendQuery(con,sql)  
}
on.exit(dbDisconnect(con))

