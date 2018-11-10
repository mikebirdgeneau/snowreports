
plot.HRDPS.local<-function(local)
{
  # Fetch Data from Models
  model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
  model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1),count=c(24)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.lat<-local$y[1]
  tmp.long<-local$x[1]
  tmp.height<-ncvar_get(model,"HGT_surface",start=c(local$col[1],local$row[1],1),count=c(1,1,1))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(local$col[1],local$row[1],1),count=c(1,1,24))-273.15
  tmp.sprate<-ncvar_get(model,"SPRATE_surface",start=c(local$col[1],local$row[1],1),count=c(1,1,24))
  tmp.rprate<-ncvar_get(model,"RPRATE_surface",start=c(local$col[1],local$row[1],1),count=c(1,1,24))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(local$col[1],local$row[1],1),count=c(1,1,24))
  tmp.clouds<-ncvar_get(model,"TCDC_surface",start=c(local$col[1],local$row[1],1),count=c(1,1,24))/100
  tmp.wind<-ncvar_get(model,"WIND_9950Etalevel",start=c(local$col[1],local$row[1],1),count=c(1,1,24))*3.6
  tmp.winddir<-ncvar_get(model,"WDIR_9950Etalevel",start=c(local$col[1],local$row[1],1),count=c(1,1,24))
  
  # Generate Plots
  plot.data<-data.frame(
    resort=local$resort[1],
    date=tmp.date,
    lat=tmp.lat,
    long=tmp.long,
    surf.elev=tmp.height,
    temperature=tmp.temperature,
    snow=tmp.sprate,
    rain=tmp.rprate,
    apcp=tmp.apcp,
    clouds=tmp.clouds,
    wind=tmp.wind,
    winddir=tmp.winddir)
  suppressWarnings(rm(tmp.apcp,tmp.clouds,tmp.date,tmp.press,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir))

    resort.data<-subset(plot.data,resort==local$resort[1])
    resort.data$date<-as.POSIXct(resort.data$date,tz=cur.tz)
    resort.data$hour<-hour(resort.data$date)
    resort.data$daylight<-abs(resort.data$hour-12)^2
    resort.data$quad<-c(rep(c(1),times=6),rep(c(2),times=6),rep(c(3),times=6),rep(c(4),times=6))
    rects<-data.frame(xstart=resort.data$date[-24],xend=resort.data$date[-1],col=resort.data$daylight[-24]/11)
    rects$xstart[1]<-rects$xstart[1]-3900
    rects$xend[23]<-rects$xend[23]+3900
    resort.data$winddir.rad<-resort.data$winddir*pi/180
    resort.data$windfrac<-resort.data$wind/max(resort.data$wind)
    pcp.data<-data.table(subset(resort.data,select=c(resort,date,snow,rain)))
    setnames(pcp.data,c("snow","rain"),c("Cum. Snow","Cum. Rain"))
    pcp.data[,snow.inst:=diff(c(0,`Cum. Snow`)),]
    pcp.data[,rain.inst:=diff(c(0,`Cum. Rain`)),]
    setnames(pcp.data,c("snow.inst","rain.inst"),c("Snow","Rain"))
    setkeyv(pcp.data,c("resort","date"))
  
    pcp.data<-data.table(melt(pcp.data,id.vars=c("resort","date")))
  
    p1<-ggplot()+
      geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
      scale_alpha_continuous(range=c(0,0.25),guide="none")+
      geom_bar(data=pcp.data[variable %in% c("Snow","Rain")],stat="identity",aes(x=date,y=value,fill=variable),position="stack")+
      geom_line(data=pcp.data[variable %in% c("Cum. Snow","Cum. Rain")],aes(x=date,y=value,colour=variable))+
    geom_point(data=pcp.data[variable %in% c("Cum. Snow","Cum. Rain")],aes(x=date,y=value,colour=variable))+
      #opts(title=paste0("Forecast Snowfall Accumulation at ",resort.data$resort[1]))+
      xlab("")+ylab("Accumulation (cm snow, mm rain)\n(cm @ 100kg/m3)")+
      scale_fill_manual(name="Legend",values = c("Snow" = "steelblue2","Cum. Snow" = "steelblue2", "Rain" = "lightblue","Cum. Rain" = "lightblue"))+
      scale_colour_manual(name="",values = c("Snow" = "steelblue2","Cum. Snow" = "steelblue2", "Rain" = "lightblue","Cum. Rain" = "lightblue"))+
      #xlim(min(resort.data$date),max(resort.data$date))+
      ylim(0,max(c(5,resort.data$snow,resort.data$rain.acc,resort.data$snow.acc+resort.data$rain.acc)))
    
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
      theme(panel.background=element_rect(fill = "white", colour = NA),
           panel.grid.major=element_line(colour="darkgrey",size=0.5),
           axis.text.x=element_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("Wind")
    
    wind.data<-subset(resort.data, quad == 2)
    wind.data<-wind.data[complete.cases(wind.data),]
    p4<-ggplot(wind.data,aes(x=winddir,y=wind))+
      geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
      scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
      scale_y_continuous(limits=c(0,wind.pmax))+
      theme(panel.background=element_rect(fill = "white", colour = NA),
           panel.grid.major=element_line(colour="darkgrey",size=0.5),
           axis.text.x=element_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("")
    
    wind.data<-subset(resort.data, quad == 3)
    wind.data<-wind.data[complete.cases(wind.data),]
    p5<-ggplot(wind.data,aes(x=winddir,y=wind))+
      geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
      scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
      scale_y_continuous(limits=c(0,wind.pmax))+
      theme(panel.background=element_rect(fill = "white", colour = NA),
           panel.grid.major=element_line(colour="darkgrey",size=0.5),
           axis.text.x=element_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("")
    
    wind.data<-subset(resort.data, quad == 4)
    wind.data<-wind.data[complete.cases(wind.data),]
    p6<-ggplot(wind.data,aes(x=winddir,y=wind))+
      geom_bar(fill="dodgerblue",stat="identity",width=1)+coord_polar()+
      scale_x_continuous(limits=c(0,360),breaks=c(0,45,90,135,180,225,270,315),labels=c("N","NE","E","SE","S","SW","W","NW"))+
      scale_y_continuous(limits=c(0,wind.pmax))+
      theme(panel.background=element_rect(fill = "white", colour = NA),
           panel.grid.major=element_line(colour="darkgrey",size=0.5),
           axis.text.x=element_text(colour="black"))+xlab(paste0(wind.data$hour[1],"-",wind.data$hour[6],"h"))+ylab("")
    
    elev<-unique(resort.data$surf.elev)
    cat(paste0("* Generating plot for ",local$resort," (",elev,"m)","...\n"))
    file.loc=str_replace_all(paste0("graphs/24h_forecast_",local$resort,".png")," ","_")
    png(filename=file.loc,width=790,height=700,units="px",res=72,type="cairo")
    #png(filename=file.loc,width=790,height=700,units="px",res=72) # Run on local machine
    grid.arrange(p1,p2,arrangeGrob(p3,p4,p5,p6, widths=c(1/4, 1/4, 1/4, 1/4), nrow=1), nrow=3,top = paste0("\nWeather Forecast for 24h Period at ",resort.data$resort[1]," (",elev,"m) - Snow-Reports.ca"))
    #grid.arrange(p1,p2,arrangeGrob(p3,p4,p5,p6, widths=c(1/4, 1/4, 1/4, 1/4), nrow=1), nrow=3,main = paste0("\nWeather Forecast for 24h Period at ",resort.data$resort[1]," - Snow-Reports.ca"))
    dev.off()
  
}
