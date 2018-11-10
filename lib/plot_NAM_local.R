
windDir <- function(u, v) {
  library(data.table)
  dt.wind<-data.table(u=u,v=v)
  dt.wind[v>0,wdir:=((180 / pi) * atan(u/v) + 180),]
  dt.wind[u < 0 & v < 0,wdir:=((180 / pi) * atan(u/v) + 180),]
  dt.wind[u > 0 & v < 0,wdir:= ((180 / pi) * atan(u/v) + 360),]
  return(dt.wind$wdir)
}

plot.NAM.local<-function(poi="Lake Louise",type="resorts",limit.data=FALSE)
{
  local.cells<-getCells.NAM(type=type)
  local<-local.cells[which(local.cells$resort==poi),]
  
  model <- nc_open(paste0(path,"cache/noaa_nam/noaa_nam.nc"),readunlim=TRUE)
  #dims=dim(ncvar_get(model0,"LAND_surface"))
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))-273.15
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))
  
  tmp.sprate<-ncvar_get(model,"CSNOW_surface",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))
  tmp.rprate<-ncvar_get(model,"CRAIN_surface",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))
  
  tmp.sprate <- tmp.sprate * tmp.apcp
  tmp.rprate <- tmp.rprate * tmp.apcp

  tmp.clouds<-ncvar_get(model,"TCDC_entireatmosphere_consideredasasinglelayer_",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))/100

  tmp.wind<-ncvar_get(model,"GUST_surface",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))*3.6 # Convert to km/h
  tmp.windu<-ncvar_get(model,"UGRD_1000mb",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))*3.6
  tmp.windv<-ncvar_get(model,"VGRD_1000mb",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))*3.6
  tmp.winddir<-windDir(tmp.windu,tmp.windv)
  tmp.windmag<-sqrt(tmp.windu^2+tmp.windv^2)
  rm(tmp.windu,tmp.windv)
  
  #tmp.press.sl<-ncvar_get(model,"PRMSL_meansealevel",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))/1E3
  #tmp.dpt<-ncvar_get(model,"DPT_2maboveground",start=c(local$col,local$row,1),count=c(1,1,length(tmp.date)))-273.15
  
  tmp<-data.frame(
    resort=local$resort,
    elev=ncvar_get(model,"HGT_surface",start=c(local$col,local$row,1),count=c(1,1,1)),
    date=tmp.date,
    temperature=tmp.temperature,
    snow=tmp.sprate,
    rain=tmp.rprate,
    apcp=tmp.apcp,
    clouds=tmp.clouds,
    wind=tmp.wind,
    winddir=tmp.winddir#,
    #press=tmp.press.sl,
    #dpt=tmp.dpt
    )
  rm(tmp.apcp,tmp.clouds,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir
     #,tmp.dpt,tmp.press.sl
     )
  resort.data<-data.table(tmp)
  rm(tmp)
  
  
  
  if(limit.data==TRUE){
    resort.data<-resort.data[date>=Sys.time(),]
    resort.data$snow.inst<-c(0,diff(resort.data$snow))
    resort.data$rain.inst<-c(0,diff(resort.data$rain))
  } else {
    resort.data$snow.inst<-c(resort.data$snow[1],diff(resort.data$snow))
    resort.data$rain.inst<-c(resort.data$rain[1],diff(resort.data$rain))
  }
  
  resort.data[snow.inst<0,snow.inst:=0,]
  resort.data[rain.inst<0,rain.inst:=0,]
  
  resort.data[,snow:=cumsum(snow.inst),]
  resort.data[,rain:=cumsum(rain.inst),]
  
  #resort.data[,relhum:=calc.relhum(tmp=temperature,dpt=dpt),]
  
  resort.data[,hour:=hour(date),]
  resort.data[,daylight:=abs(hour-12)^2+25,]
  resort.data[daylight<=0,daylight:=0]
  resort.data[daylight>=115,daylight:=115,]
  resort.data[,daylight:=daylight/max(daylight)*100,]
  resort.data[,quad:=cut(as.numeric(date-min(date))/as.numeric((max(date)-min(date))),breaks = 4,labels = FALSE),]
  #resort.data$quad<-c(rep(c(1),times=4),rep(c(2),times=4),rep(c(3),times=4),rep(c(4),times=4))
  resort.data$winddir.rad<-resort.data$winddir*pi/180
  resort.data$windfrac<-resort.data$wind/max(resort.data$wind)
  pcp.data<-subset(resort.data,select=c(resort,date,snow,rain))
  pcp.data.inst<-subset(resort.data,select=c(resort,date,snow.inst,rain.inst))
  setnames(pcp.data,c("snow","rain"),c("Cum. Snow","Cum. Rain"))
  setnames(pcp.data.inst,c("snow.inst","rain.inst"),c("Snow","Rain"))
  pcp.data<-melt(pcp.data,id.vars=c("resort","date"))
  pcp.data.inst<-melt(pcp.data.inst,id.vars=c("resort","date"))
  
  # Set-up Shading for Daylight
  rects<-data.frame(xstart=resort.data$date[1:length(tmp.date)-1],xend=resort.data$date[2:length(tmp.date)],col=resort.data$daylight[1:length(tmp.date)-1]/max(resort.data$daylight))
  rects$xstart[1]<-rects$xstart[1]-3900
  rects$xend[length(tmp.date)-1]<-rects$xend[length(tmp.date)-1]+3900
  
  # Set-up Data Structure for Tables, Forecast, etc.
  p1<-ggplot()+
    geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
    scale_alpha_continuous(range=c(0,0.15),guide="none")+
    geom_bar(data=pcp.data.inst,stat="identity",aes(x=date,y=value,fill=variable),position="stack")+
    geom_line(data=pcp.data,aes(x=date,y=value,colour=variable))+
    geom_point(data=pcp.data,aes(x=date,y=value,colour=variable))+
    ggtitle(paste0("Forecast Precipitation for ",resort.data$resort[1]," at ",round(resort.data$elev[1],0),"m"))+
    xlab("")+ylab("Precipitation\n(mm, water equivalent)")+
    scale_fill_manual(name="Legend",values = c("Snow" = "steelblue2","Rain" = "lightblue"))+
    scale_colour_manual(name="",values=c("Cum. Snow" = "steelblue2","Cum. Rain" = "lightblue"))+
    #xlim(min(resort.data$date),max(resort.data$date))+
    ylim(0,max(5,resort.data$snow,resort.data$rain))
  
  p2<-ggplot()+
    geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
    scale_alpha_continuous(range=c(0,0.15),guide="none")+
    geom_line(data=resort.data,aes(x=date,y=temperature))+
    #xlab("")+
    xlab(paste0("Forecast Time (",format(Sys.time(),"%Z",tz="America/Edmonton"),") - Source: NOAA/NCEP North American Mesoscale (NAM)"))+
    ylab("Temperature\n(Celsius)")+
    ylim(min(-10,resort.data$temperature),max(0,resort.data$temperature))+
    ggtitle(paste0("Forecast Temperatures for ",resort.data$resort[1]," at ",round(resort.data$elev[1],0),"m"))+
    geom_hline(y=0,aes(colour="Freezing"),colour="Red")+
    scale_color_gradientn(name="Cloud Cover\n(%)",colours=c("gold1","khaki","ivory3","ivory4"), limits=c(0,100),guide="colourbar")+
    geom_point(data=resort.data,aes(x=date,y=temperature,colour=clouds*100),size=4)
  
  require(circular)
  wind.data<-resort.data[,list(
    wind=mean(wind),
    max.wind=max(wind),
    winddir=mean(circular(winddir,units="degrees",template="geographics")),
    winddir.min=min(winddir),
    winddir.max=max(winddir),
    winddir.rad=mean(winddir.rad),
    hour.max=max(hour),
    hour.min=min(hour),
    date=strftime(min(date),format="%a %b %d")
  ),by=c("quad")]
  # Check for too wide / small wind arrows & adjust if necessary.
  wind.data[(winddir.max-winddir.min)>(winddir.min-(winddir.max-360)),winddir.max:=winddir.max-360]
  wind.data[,wind.width:=abs(winddir.max-winddir.min)]
  wind.data[wind.width<10,wind.width:=10]
  wind.data<-wind.data[complete.cases(wind.data),]
  wind.pmax=max(resort.data$wind) #35
  p3<-ggplot(wind.data[quad==1,],aes(x=winddir,y=wind))+
    geom_bar(fill="darkgrey",stat="identity",aes(width=wind.width))+coord_polar()+
    #geom_area(fill="dodgerblue")+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    theme_minimal()+
    xlab(paste0(wind.data[quad==1,]$date,"\n",wind.data[quad==1,]$hour.min,":00 -",wind.data[quad==1,]$hour.max,":00h"))+
    ylab("Wind (km/h)")+
    theme(panel.grid=element_line(colour="black"))
  
  p4<-ggplot(wind.data[quad==2,],aes(x=winddir,y=wind))+
    geom_bar(fill="darkgrey",stat="identity",aes(width=winddir.max-winddir.min))+coord_polar()+
    #geom_area(fill="dodgerblue")+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    theme_minimal()+
    xlab(paste0(wind.data[quad==2,]$date,"\n",wind.data[quad==2,]$hour.min,":00 -",wind.data[quad==2,]$hour.max,":00h"))+ylab("")+
    theme(panel.grid=element_line(colour="black"))
  
  p5<-ggplot(wind.data[quad==3,],aes(x=winddir,y=wind))+
    geom_bar(fill="darkgrey",stat="identity",aes(width=winddir.max-winddir.min))+coord_polar()+
    #geom_area(fill="dodgerblue")+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    theme_minimal()+
    xlab(paste0(wind.data[quad==3,]$date,"\n",wind.data[quad==3,]$hour.min,":00 -",wind.data[quad==3,]$hour.max,":00h"))+ylab("")+
    theme(line=element_line(colour="black"))
  
  p6<-ggplot(wind.data[quad==4,],aes(x=winddir,y=wind))+
    geom_bar(fill="darkgrey",stat="identity",aes(width=winddir.max-winddir.min))+coord_polar()+
    #geom_area(fill="dodgerblue")+coord_polar()+
    scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
    scale_y_continuous(limits=c(0,wind.pmax))+
    theme_minimal()+
    xlab(paste0(wind.data[quad==4,]$date,"\n",wind.data[quad==4,]$hour.min,":00 -",wind.data[quad==4,]$hour.max,":00h"))+ylab("")+
    theme(panel.grid=element_line(colour="black"))
  
  dir.create("graphs",showWarnings = FALSE)
  
  png(filename=paste0(path,"graphs/84h_nam_forecast_",str_replace_all(resort.data$resort[1]," ","_"),"_precipitation.png"),width=600,height=200)
  print(p1)
  dev.off()
  png(filename=paste0(path,"graphs/84h_nam_forecast_",str_replace_all(resort.data$resort[1]," ","_"),"_temperature.png"),width=600,height=200)
  print(p2)
  dev.off()
  png(filename=paste0(path,"graphs/84h_nam_forecast_",str_replace_all(resort.data$resort[1]," ","_"),"_wind.png"),width=600,height=150)
  grid.arrange(p3,p4,p5,p6,nrow=1)
  dev.off()
  
  png(filename=paste0(path,"graphs/84h_nam_forecast_",str_replace_all(resort.data$resort[1]," ","_"),".png"),width=790,height=700)
  grid.arrange(p1,p2,arrangeGrob(p3,p4,p5,p6,nrow=1),nrow=3)
  dev.off()
  
  dir.create("cache/plots/",showWarnings = FALSE,recursive = TRUE)
  save(p1,p2,p3,p4,p5,p6,file=paste0("cache/plots/noaa_nam_",str_replace_all(str_replace_all(poi," ","_"),"\\.",""),"_plots.Rda"))
  
  return(resort.data)
}