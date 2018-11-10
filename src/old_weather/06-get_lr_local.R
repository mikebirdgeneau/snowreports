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
require(reshape)

# Get current date in UTC
date=format(as.POSIXct(Sys.time()-7*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")

model0 <- nc_open("results/lrmodel_000.nc")
model <- nc_open("results/lrmodel.nc",readunlim=FALSE)
#dims=dim(ncvar_get(model0,"LAND_surface"))
date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")

# Add annotations for resorts
resorts<-data.frame(c("Apex",-119.901258,49.383206),
                    c("Big White",-118.936465,49.718226),
                    c("Castle",-114.414854,49.318039),
                    c("Fernie",-115.088050,49.462611),
                    c("Fortress",-115.198479,50.824895),
                    c("Kicking Horse",-117.078433,51.274922),
                    c("Kimberley",-116.016485,49.682298),
                    c("Lake Louise",-116.139285,51.466492),
                    c("Marmot",-118.084851,52.800858),
                    c("Mt Washington",-125.310683,49.741935),
                    c("Nakiska",-115.151383,50.942581),
                    c("Norquay",-115.600661,51.203489),
                    c("Panorama",-116.237474,50.457003),
                    c("Powder King",-122.613350,55.358821),
                    c("Red Mountain",-117.822502,49.104072),
                    c("Revelstoke",-118.092132,50.962857),
		    c("Shames",-128.961203,54.488983),
                    c("Silver Star",-119.061755,50.358843),
                    c("Sun Peaks",-119.885167,50.882157),
                    c("Sunshine",-115.781810,51.079066),
                    c("Whistler",-122.953433,50.107467),
                    c("Whitewater",-117.145346,49.443357),
                    c("Whitefish",-114.353560,48.486294),
                    c("Rogers Pass",-117.52000,51.300418),
                    c("Black Prince",-115.2367,50.706977),
                    c("Bow Summit",-116.498122,51.716055),
                    c("Highwood Pass",-114.967028,50.594155),
                    c("Kootenay Pass",-117.040151,49.056480),
                    c("Canmore",-115.357831,51.085274),
                    c("Calgary",-114.081533,51.053464),
                    c("Banff",-115.570893,51.178401),
                    c("Asulkan",-117.463,51.212),
                    c("Amiskwi Lodge",-116.670556,51.618070),
                    c("Pemberton Icecap",-123.2646,50.3568),
                    c("Waterton",-114.025,49.0597),
                    c("Jumbo Glacier",-116.5611,50.4071),
                    c("Burstall Pass",-115.3471,50.772684),
                    c("Assiniboine",-115.6204,50.913072),
                    c("Vermillion Peak",-116.1285,51.1568),
                    c("Parker Ridge",-117.1298,52.18588),
                    c("Little Yoho",-116.5643,51.5256),
                    c("Lake OHara",-116.3444,51.3556),
                    c("49 Degrees North",-117.56376,48.340016),
                    c("Windy Peak",-114.3398,50.3934))

rownames(resorts)<-c("Resort","x","y")
colnames(resorts)<-NULL
resorts<-data.frame(t(resorts))
resorts$x=as.numeric(as.character(resorts$x))
resorts$y=as.numeric(as.character(resorts$y))
#write.csv(resorts,file="data/resorts.csv",row.names=FALSE)
coordinates(resorts)<-c('x','y')
proj4string(resorts)='+proj=longlat +ellps=GRS80 +towgs84'

# Set-up Grid Cells
x <- as.vector(ncvar_get(model, "longitude"))-360
y <- as.vector(ncvar_get(model, "latitude"))
coords=data.frame(x=x,y=y)
coordinates(coords)=~x+y
proj4string(coords)<-CRS("+proj=latlong")
#coords<-spTransform(coords,CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=249 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"))
grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(10000,10000),c(935,824)))
#grid=SpatialGrid(GridTopology(bbox(coords)[,1],c(15000,15000),c(493,399)))
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
resort.cells$row<-ceiling(resort.cells$cellno/935)
resort.cells$col<-935-(resort.cells$row*935-resort.cells$cellno)

# Get data by resort, over 24 hr period.
plot.data<-data.frame()
for(i in 1:nrow(resort.cells)){
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1),count=c(16)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))-273.15
  tmp.sprate<-ncvar_get(model,"SPRATE_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))
  tmp.rprate<-ncvar_get(model,"RPRATE_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))
  tmp.clouds<-ncvar_get(model,"TCDC_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))/100
  tmp.wind<-ncvar_get(model,"WIND_10maboveground",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))*3.6
  tmp.winddir<-ncvar_get(model,"WDIR_10maboveground",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))
  tmp.press<-ncvar_get(model,"PRES_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,16))/1E3
  tmp<-data.frame(
    resort=resort.cells$resort[i],
    date=tmp.date,
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
  resort.data$quad<-c(rep(c(1),times=4),rep(c(2),times=4),rep(c(3),times=4),rep(c(4),times=4))
  #rects<-data.frame(xstart=resort.data$date[-16],xend=resort.data$date[-1],col=resort.data$daylight[-16]/11)
  #rects$xstart[1]<-rects$xstart[1]-3900
  #rects$xend[23]<-rects$xend[23]+3900
  resort.data$winddir.rad<-resort.data$winddir*pi/180
  resort.data$windfrac<-resort.data$wind/max(resort.data$wind)
  pcp.data<-subset(resort.data,select=c(resort,date,snow.acc,rain.acc))
  setnames(pcp.data,c("snow.acc","rain.acc"),c("Cum. Snow","Cum. Rain"))
  pcp.data<-melt(pcp.data,id.vars=c("resort","date"))
  p1<-ggplot()+
    #geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
    scale_alpha_continuous(range=c(0,0.25),guide="none")+
    geom_bar(data=pcp.data,stat="identity",aes(x=date,y=value,fill=variable),position="stack")+
    #opts(title=paste0("Forecast Snowfall Accumulation at ",resort.data$resort[1]))+
    xlab("")+ylab("Accumulation (cm snow, mm rain)\n(cm @ 100kg/m3)")+
    scale_fill_manual(name="Legend",values = c("Cum. Snow" = "steelblue2","Cum. Rain" = "lightblue"))+
    #xlim(min(resort.data$date),max(resort.data$date))+
    ylim(0,max(5,resort.data$snow.acc,resort.data$rain.acc,resort.data$snow.acc+resort.data$rain.acc))
  
  p2<-ggplot()+
    #geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
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
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[4],"h"))+ylab("Wind")
  
  wind.data<-subset(resort.data, quad == 2)
  wind.data<-wind.data[complete.cases(wind.data),]
  p4<-ggplot(wind.data,aes(x=winddir,y=wind))+
    geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    opts(panel.background=theme_rect(fill = "white", colour = NA),
         panel.grid.major=theme_line(colour="darkgrey",size=0.5),
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[4],"h"))+ylab("")
  
  wind.data<-subset(resort.data, quad == 3)
  wind.data<-wind.data[complete.cases(wind.data),]
  p5<-ggplot(wind.data,aes(x=winddir,y=wind))+
    geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    opts(panel.background=theme_rect(fill = "white", colour = NA),
         panel.grid.major=theme_line(colour="darkgrey",size=0.5),
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[4],"h"))+ylab("")
  
  wind.data<-subset(resort.data, quad == 4)
  wind.data<-wind.data[complete.cases(wind.data),]
  p6<-ggplot(wind.data,aes(x=winddir,y=wind))+
    geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    opts(panel.background=theme_rect(fill = "white", colour = NA),
         panel.grid.major=theme_line(colour="darkgrey",size=0.5),
         axis.text.x=theme_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[4],"h"))+ylab("")
  
  cat(paste0("* Generating plot for ",resortlist[i],"...\n"))
  file.loc=str_replace_all(paste0("graphs/48h_forecast_",resortlist[i],"_tmp.png")," ","_")
  png(filename=file.loc,width=790,height=700,units="px",res=72,type="cairo")
  #png(filename=file.loc,width=790,height=700,units="px",res=72) # Run on local machine
  grid.arrange(p1,p2,arrangeGrob(p3,p4,p5,p6, widths=c(1/4, 1/4, 1/4, 1/4), nrow=1), nrow=3,main = paste0("\nWeather Forecast for 48h Period at ",resort.data$resort[1]," - Snow-Reports.ca"))
  dev.off()
}

gc()

for(i in 1:length(resortlist)){
  file.loc=str_replace_all(paste0("graphs/48h_forecast_",resortlist[i],"_tmp.png")," ","_")
  file.dest=str_replace_all(paste0("graphs/48h_forecast_",resortlist[i],".png")," ","_")
  system(paste0("mv ",file.loc," ",file.dest,"\n"))
}
