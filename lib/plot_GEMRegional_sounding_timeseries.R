# Generates a heat map showing atmospheric conditions over the forecast period (e.g. 48h)

get.GEMRegional.soundingData <- function(poi="Lake Louise",type="resorts",limit.data=FALSE){
  
  local.cells<-getCells.GEMRegional(type=type)
  local<-local.cells[which(local.cells$resort==poi),]
  
  model0 <- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional_000.nc"))
  model <- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional.nc"),readunlim=TRUE)
  model.ext<- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional_ext.nc"),readunlim=TRUE)
  
  #dims=dim(ncvar_get(model0,"LAND_surface"))
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  
  # Get data to create sounding plot
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1),count=c(16)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.press.sfc<-ncvar_get(model,"PRES_surface",start=c(local$col,local$row,1),count=c(1,1,16))/1E3
  tmp.hgt.1000<-ncvar_get(model.ext,"HGT_1000mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.hgt.900<-ncvar_get(model.ext,"HGT_900mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.hgt.800<-ncvar_get(model.ext,"HGT_800mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.hgt.700<-ncvar_get(model.ext,"HGT_700mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.hgt.600<-ncvar_get(model.ext,"HGT_600mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.hgt.500<-ncvar_get(model.ext,"HGT_500mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.temp.1000<-ncvar_get(model.ext,"TMP_1000mb",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  tmp.temp.900<-ncvar_get(model.ext,"TMP_900mb",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  tmp.temp.800<-ncvar_get(model.ext,"TMP_800mb",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  tmp.temp.700<-ncvar_get(model.ext,"TMP_700mb",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  tmp.temp.600<-ncvar_get(model.ext,"TMP_600mb",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  tmp.temp.500<-ncvar_get(model.ext,"TMP_500mb",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  tmp.depr.1000<-ncvar_get(model.ext,"DEPR_1000mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.depr.900<-ncvar_get(model.ext,"DEPR_900mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.depr.800<-ncvar_get(model.ext,"DEPR_800mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.depr.700<-ncvar_get(model.ext,"DEPR_700mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.depr.600<-ncvar_get(model.ext,"DEPR_600mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.depr.500<-ncvar_get(model.ext,"DEPR_500mb",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.precip<-ncvar_get(model,"APCP_surface",start=c(local$col,local$row,1),count=c(1,1,16))
  
  plot.data<-data.table(rbind(
    data.frame(DATE=tmp.date,press=c(1000),HGT=tmp.hgt.1000,temp=tmp.temp.1000,DEPR=tmp.depr.1000,APCP=tmp.precip),
    data.frame(DATE=tmp.date,press=c(900),HGT=tmp.hgt.900,temp=tmp.temp.900,DEPR=tmp.depr.900,APCP=tmp.precip),
    data.frame(DATE=tmp.date,press=c(800),HGT=tmp.hgt.800,temp=tmp.temp.800,DEPR=tmp.depr.800,APCP=tmp.precip),
    data.frame(DATE=tmp.date,press=c(700),HGT=tmp.hgt.700,temp=tmp.temp.700,DEPR=tmp.depr.700,APCP=tmp.precip),
    data.frame(DATE=tmp.date,press=c(600),HGT=tmp.hgt.600,temp=tmp.temp.600,DEPR=tmp.depr.600,APCP=tmp.precip),
    data.frame(DATE=tmp.date,press=c(500),HGT=tmp.hgt.500,temp=tmp.temp.500,DEPR=tmp.depr.500,APCP=tmp.precip)))
  
  suppressWarnings(rm(tmp.press.sfc,tmp.hgt.1000,tmp.hgt.900,tmp.hgt.800,tmp.hgt.700,tmp.hgt.600,tmp.hgt.500,tmp.temp.1000,tmp.temp.900,tmp.temp.800,tmp.temp.700,tmp.temp.600,tmp.temp.500,tmp.depr.1000,tmp.depr.900,tmp.depr.800,tmp.depr.700,tmp.depr.600,tmp.depr.500,tmp.precip))
  
  plot.data[,dewpt:=temp-DEPR,]
  plot.data[,precip:=ifelse(DEPR<=0.5,1,0)]
  
  plot.data[,freeze.level:=0,]
  plot.data[,freeze.level:=max(ifelse(temp>0,HGT,0)),by=c("DATE")]
  plot.data[,freeze.level:=mean(ifelse(freeze.level>0,freeze.level,NA),na.rm=TRUE),by=c("DATE")]
  plot.data[is.na(freeze.level),freeze.level:=0,]
  plot.data[,inversion.flag:=FALSE,]
  plot.data[,freeze.inversion.flag:=FALSE,]
  setkeyv(plot.data,c("DATE","HGT"))
  plot.data[HGT<freeze.level,freeze.inversion.flag:=ifelse(min(temp)>=0,FALSE,TRUE),by=c("DATE")]
  plot.data[,inversion.flag:=ifelse(c(-1e-12,diff(temp))>3,TRUE,FALSE),by=c("DATE")]
  plot.data[,sum.inversion.flag:=sum(inversion.flag),by=c("DATE")]
  plot.data[,sum.freeze.inversion.flag:=sum(freeze.inversion.flag),by=c("DATE")]
  plot.data[sum.freeze.inversion.flag>0,freeze.level:=0,by=c("DATE")]
  plot.data[,freeze.level:=max(freeze.level),by=c("DATE")]
  
  # Test using kriging instead of linear approximating to provide better trends detail (over time and height)
  require(gstat)
  dt.spatial<-data.frame(plot.data)
  dt.spatial$numdate<-as.numeric(dt.spatial$DATE)/34.5
  coordinates(dt.spatial)<- ~numdate+HGT
  
  newgrid<-expand.grid(numdate=seq(min(as.numeric(plot.data$DATE)),max(as.numeric(plot.data$DATE)),length.out=48)/34.5,HGT=seq(floor(min(plot.data$HGT)/100)*100,ceiling(max(plot.data$HGT)/100)*100,by=100))
  elev.plot<-data.table(newgrid)
  coordinates(newgrid)<- ~numdate+HGT
  gridded(newgrid)<-TRUE
  
  # Don't use fit variogram! Results too unstable!!
  v<-variogram(temp~HGT+numdate,dt.spatial,alpha=c(0,90))
  v.fit<-fit.variogram(v,vgm(psill=1.5,model="Gau",range=4000,nugget=0.1))
  if(v.fit$psill[1]==0){v.fit$psill[1]<-0.1}
  elev.plot$temp<-krige(temp~HGT+numdate,dt.spatial,newgrid,model=v.fit,nmax=12)$var1.pred
  #plot(v,v.fit)
  #spplot(krige(temp~HGT+numdate,dt.spatial,newgrid,model=v.fit,nmax=12),c("var1.pred"))
  
  v<-variogram(DEPR~HGT+numdate,dt.spatial)
  v.fit<-fit.variogram(v,vgm(psill=11,model="Gau",range=2000,nugget=0))
  if(v.fit$psill[1]==0){v.fit$psill[1]<-1}
  if(v.fit$range[2]<=750){
    message(paste0("Using default VGM due to short fit range! (",round(v.fit$range[2],0),"m)"))
    v.fit<-vgm(psill=11,range=1000,nugget=1,model="Gau")
  }
  elev.plot$depr<-krige(DEPR~HGT+numdate,dt.spatial,newgrid,model=v.fit,nmax=36)$var1.pred
  elev.plot[depr<=0,depr:=0,]
  
  #plot(v,v.fit)
  #spplot(krige(DEPR~HGT+numdate,dt.spatial,newgrid,model=v.fit),c("var1.pred"))
  
  v<-variogram(press~HGT+numdate,dt.spatial,alpha=c(0,90))
  v.fit<-fit.variogram(v,vgm(psill=300,model="Gau",range=2000,nugget=0))
  v.fit$psill[1]<-0.25
  elev.plot$press<-krige(press~HGT+numdate,dt.spatial,newgrid,model=v.fit,nmax=36)$var1.pred
  #plot(v,v.fit)
  #spplot(krige(press~HGT+numdate,dt.spatial,newgrid,model=v.fit),c("var1.pred"))
  
  setnames(elev.plot,c("HGT"),c("elev"))
  elev.plot$date<-as.POSIXct(elev.plot$numdate*34.5,origin="1970-01-01")
  elev.plot$numdate<-NULL
  
  #rm(newgrid,dt.spatial)
  
  # TEST KRIGING FOR FREEZING ELEVATION
  elev.plot[,freeze.level:=0,]
  elev.plot[,freeze.level:=max(ifelse(temp>0,elev,0)),by=c("date")]
  elev.plot[,freeze.level:=mean(ifelse(freeze.level>0,freeze.level,NA),na.rm=TRUE),by=c("date")]
  elev.plot[is.na(freeze.level),freeze.level:=0,]
  elev.plot[,inversion.flag:=FALSE,]
  elev.plot[elev<freeze.level,inversion.flag:=ifelse(min(temp)>=0,FALSE,TRUE),by=c("date")]
  elev.plot[,sum.inversion.flag:=sum(inversion.flag),by=c("date")]
  elev.plot[sum.inversion.flag>0,freeze.level:=0,by=c("date")]
  elev.plot[,freeze.level:=max(freeze.level),by=c("date")]
  
  #freeze.level<-plot.data[,list(freeze.level=freeze.level),by=c("DATE")]
  freeze.level<-elev.plot[,list(freeze.level=mean(freeze.level,na.rm=TRUE)),by=c("date")]
  setnames(freeze.level,c("date"),c("DATE"))
  
  #elev.plot<-data.table(expand.grid(date=unique(plot.data$DATE),elev=pretty(plot.data$HGT,n=20)))
  #elev.plot[,temp:=approx(plot.data[DATE==date,]$HGT,y=plot.data[DATE==date,]$temp,xout=elev)$y,by=c("date")]
  #elev.plot[,depr:=approx(plot.data[DATE==date,]$HGT,y=plot.data[DATE==date,]$DEPR,xout=elev)$y,by=c("date")]
  #elev.plot[,dewpt:=approx(plot.data[DATE==date,]$HGT,y=plot.data[DATE==date,]$dewpt,xout=elev)$y,by=c("date")]
  #elev.plot[,precip:=round(approx(plot.data[DATE==date,]$HGT,y=plot.data[DATE==date,]$precip,xout=elev)$y,0),by=c("date")]
  
  elev.plot[,abv.zero:=ifelse(temp>0,"red","lightgrey"),]
  
  elev.info<-dt.local.cell.info[resort==poi,]
  if(!is.finite(elev.info$valley.btm)){
    message("No Valley Bottom Set. No plot generated.")
    return(FALSE)
  }
  elev.info[fcst.elev<valley.btm,valley.btm:=fcst.elev,]
  
  if(nrow(elev.info)>0){
    if(is.finite(elev.info$valley.btm)){
      elev.plot<-elev.plot[elev>=elev.info$valley.btm,]
    }
  }
  
  
  elev.plot[,min(freeze.level),by=c("date")]
  
  # Calculate Freezing / Rain Levels, and Limit to Valley Bottom
  freeze.level[freeze.level<=elev.info$valley.btm,freeze.level:=elev.info$valley.btm,]
  freeze.level[,rain.level:=freeze.level-304.8,]
  #freeze.level[,rain.level:=predict(loess(rain.level~as.numeric(DATE)),DATE),]
  #freeze.level[,freeze.level:=predict(loess(freeze.level~as.numeric(DATE)),DATE),]
  freeze.level[freeze.level<=elev.info$valley.btm,freeze.level:=elev.info$valley.btm,]
  freeze.level[rain.level<=elev.info$valley.btm,rain.level:=elev.info$valley.btm,]
  
  return(list(elev.plot = elev.plot, freeze.level = freeze.level, elev.info = elev.info, plot.data = plot.data, date.fancy = date.fancy, model = model, model0 = model0, local = local))
  
}

plot.GEMRegional.sounding<-function(poi="Lake Louise",type="resorts",limit.data=FALSE,soundingData)
{
  require(gridExtra)
  require(directlabels)
  
  if(missing(soundingData)){
    soundingData <- get.GEMRegional.soundingData(poi = poi, type = type, limit.data = limit.data)
  }
  
  elev.plot <- soundingData$elev.plot
  freeze.level <- soundingData$freeze.level
  elev.info <- soundingData$elev.info
  plot.data <- soundingData$plot.data
  date.fancy <- soundingData$date.fancy
  model <- soundingData$model
  model0 <- soundingData$model0
  local <- soundingData$local
  
  p2<-ggplot(data=elev.plot,aes(x=date,y=elev))+geom_tile(aes(fill=depr))+theme_bw()+ylab("Elevation (mASL)")+xlab("")+scale_fill_gradientn(colours=c("steelblue4","dodgerblue","skyblue","white","orange","brown"),values=c(0,0.025,0.05,0.142,0.65,1),limits=c(0,35),name="Dewpoint\nDepression\n(C)")
  #+scale_color_gradient(low="lightgrey",high="blue",name="Precip",guide=FALSE)
  
  if(sum(ifelse(is.finite(freeze.level$freeze.level),1,0))>0){
    #p2<-p2+stat_smooth(data=freeze.level,aes(x=DATE,y=freeze.level,colour="Freezing"),se=FALSE,method="loess")+stat_smooth(data=freeze.level,aes(x=DATE,y=rain.level,colour="Rain Line"),se=FALSE,method="loess")+scale_colour_manual(values=c("Freezing" = "red","Lifts"="black","Rain Line"="firebrick"),name="")
    p2<-p2+geom_line(data=freeze.level,aes(x=DATE,y=freeze.level,colour="Freezing"))+geom_line(data=freeze.level,aes(x=DATE,y=rain.level,colour="Rain Line"))+scale_colour_manual(values=c("Freezing" = "red","Lifts"="black","Rain Line"="firebrick"),name="")
    
  }
  
  p2<-p2+geom_hline(data=elev.info,aes(yintercept=base.lift,colour="Lifts",linetype="Base"),show_guide=TRUE)+geom_hline(data=elev.info,aes(yintercept=top.lift,colour="Lifts",linetype="Top"),show_guide=TRUE)+geom_hline(data=elev.info,aes(yintercept=fcst.elev,colour="Lifts",linetype="Fcst Elev"),show_guide=TRUE)+scale_linetype_manual(values=c("Base"=1,"Top"=2,"Fcst Elev"=3),name="")+xlab("Note: Freezing elevation is approximate and does not include inversions; precipitation based on avg. surface elevation.")

  p3.colour=rev(brewer.pal(11,"Spectral"))
  p3.colour[6]="#FFFFFF"
  p3<-ggplot(data=elev.plot,aes(x=date,y=elev))+geom_tile(aes(fill=temp))+theme_bw()+ylab("Elevation (mASL)")+xlab("")+scale_fill_gradientn(colours=p3.colour,values=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.00),limits=c(-40,40),name="Temperature\n(C)")+stat_contour(aes(z=temp),binwidth=5,colour="darkgrey")+ggtitle(paste0("48h Forecast Elevation Profile at ",poi," for ",date.fancy))
  
  if(sum(ifelse(is.finite(freeze.level$freeze.level),1,0))>0){
    #p3<-p3+stat_smooth(data=freeze.level,aes(x=DATE,y=freeze.level,colour="Freezing"),se=FALSE,method="loess")+stat_smooth(data=freeze.level,aes(x=DATE,y=rain.level,colour="Rain Line"),se=FALSE,method="loess")+scale_colour_manual(values=c("Freezing" = "red","Lifts"="black","Rain Line"="firebrick"),name="")
    p3<-p3+geom_line(data=freeze.level,aes(x=DATE,y=freeze.level,colour="Freezing"))+geom_line(data=freeze.level,aes(x=DATE,y=rain.level,colour="Rain Line"))+scale_colour_manual(values=c("Freezing" = "red","Lifts"="black","Rain Line"="firebrick"),name="")
  }
  
  p3<-p3+geom_hline(data=elev.info,aes(yintercept=base.lift,colour="Lifts",linetype="Base"),show_guide=TRUE)+geom_hline(data=elev.info,aes(yintercept=top.lift,colour="Lifts",linetype="Top"),show_guide=TRUE)+geom_hline(data=elev.info,aes(yintercept=fcst.elev,colour="Lifts",linetype="Fcst Elev"),show_guide=TRUE)+scale_linetype_manual(values=c("Base"=1,"Top"=2,"Fcst Elev"=3),name="")
  
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1),count=c(16)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  tmp.sprate<-ncvar_get(model,"SPRATE_surface",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.rprate<-ncvar_get(model,"RPRATE_surface",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.clouds<-ncvar_get(model,"TCDC_surface",start=c(local$col,local$row,1),count=c(1,1,16))/100
  #tmp.wind<-ncvar_get(model,"WIND_80maboveground",start=c(local$col,local$row,1),count=c(1,1,16))*3.6 # Convert to km/h
  #tmp.winddir<-ncvar_get(model,"WDIR_80maboveground",start=c(local$col,local$row,1),count=c(1,1,16))
  tmp.wind<-ncvar_get(model,"WIND_9850Etalevel",start=c(local$col,local$row,1),count=c(1,1,16))*3.6 # Convert to km/h
  tmp.winddir<-ncvar_get(model,"WDIR_9850Etalevel",start=c(local$col,local$row,1),count=c(1,1,16))
  #tmp.press<-ncvar_get(model,"PRES_surface",start=c(local$col,local$row,1),count=c(1,1,16))/1E3
  tmp.press.sl<-ncvar_get(model,"PRMSL_meansealevel",start=c(local$col,local$row,1),count=c(1,1,16))/1E3
  tmp.dpt<-ncvar_get(model,"DPT_2maboveground",start=c(local$col,local$row,1),count=c(1,1,16))-273.15
  #tmp.sph<-ncvar_get(model,"SPFH_2maboveground",start=c(local$col,local$row,1),count=c(1,1,16))
  
  tmp<-data.frame(
    resort=local$resort,
    elev=ncvar_get(model0,"HGT_surface",start=c(local$col,local$row,1),count=c(1,1,1)),
    date=tmp.date,
    temperature=tmp.temperature,
    snow=tmp.sprate,
    rain=tmp.rprate,
    apcp=tmp.apcp,
    clouds=tmp.clouds,
    wind=tmp.wind,
    winddir=tmp.winddir,
    press=tmp.press.sl,
    dpt=tmp.dpt)
  rm(tmp.apcp,tmp.clouds,tmp.date,tmp.press.sl,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir,tmp.dpt)
  resort.data<-data.table(tmp)
  rm(tmp)

  resort.data$snow.inst<-c(resort.data$snow[1],diff(resort.data$snow))
  resort.data$rain.inst<-c(resort.data$rain[1],diff(resort.data$rain))
  
  resort.data[snow.inst<0,snow.inst:=0,]
  resort.data[rain.inst<0,rain.inst:=0,]
  
  resort.data[,snow:=cumsum(snow.inst),]
  resort.data[,rain:=cumsum(rain.inst),]
  
  resort.data[,relhum:=calc.relhum(tmp=temperature,dpt=dpt),]
  
  resort.data[,hour:=hour(date),]
  resort.data[,daylight:=abs(hour-12)^2+25,]
  resort.data[daylight<=0,daylight:=0]
  resort.data[daylight>=100,daylight:=100,]
  resort.data[,daylight:=daylight/max(daylight)*100,]
  resort.data$quad<-c(rep(c(1),times=4),rep(c(2),times=4),rep(c(3),times=4),rep(c(4),times=4))
  resort.data$winddir.rad<-resort.data$winddir*pi/180
  resort.data$windfrac<-resort.data$wind/max(resort.data$wind)
  pcp.data<-subset(resort.data,select=c(resort,date,snow,rain))
  pcp.data.inst<-subset(resort.data,select=c(resort,date,snow.inst,rain.inst))
  setnames(pcp.data,c("snow","rain"),c("Cum. Snow","Cum. Rain"))
  setnames(pcp.data.inst,c("snow.inst","rain.inst"),c("Snow","Rain"))
  pcp.data<-melt(pcp.data,id.vars=c("resort","date"))
  pcp.data.inst<-melt(pcp.data.inst,id.vars=c("resort","date"))
  
  # Set-up Shading for Daylight
  rects<-data.frame(xstart=resort.data$date[-16],xend=resort.data$date[-1],col=resort.data$daylight[-16]/max(resort.data$daylight))
  rects$xstart[1]<-rects$xstart[1]-3900
  rects$xend[15]<-rects$xend[15]+3900
  
  p1<-ggplot()+
    geom_rect(data=rects,aes(xmin=xstart,xmax=xend, ymin = -Inf, ymax = Inf,alpha=col),fill = "midnightblue")+
    scale_alpha_continuous(range=c(0,0.15),guide="none")+
    geom_bar(data=pcp.data.inst,stat="identity",aes(x=date,y=value,fill=variable),position="stack")+
    #geom_line(data=pcp.data,aes(x=date,y=value,colour=variable))+
    #geom_point(data=pcp.data,aes(x=date,y=value,colour=variable))+
    xlab("Snow-Reports.ca - Data: Environment Canada GEM Regional (12km)")+ylab("Precip (mm, H2O)")+
    scale_fill_manual(name="Precipitation",values = c("Snow" = "steelblue2","Rain" = "lightblue"))+
    #scale_colour_manual(name="",values=c("Cum. Snow" = "steelblue2","Cum. Rain" = "lightblue"))+
    #xlim(min(resort.data$date),max(resort.data$date))+
    ylim(0,max(5,max(pcp.data.inst$value,na.rm=TRUE)))+theme_bw()+
    ggtitle(paste0("Precipitation Forecast at ",elev.info$fcst.elev,"m Elevation"))
   
  graphics.off() # Ensure all graphics devices are closed before plotting
  png(filename=paste0("graphs/48h_",str_replace_all(string=poi,pattern=" ","_"),"_sounding.png"),width=790,height=790)
  grid.arrange(p3,p2,p1,nrow=3,heights=c(0.4,0.4,0.2))#,main=paste0("\n",poi,": Temperature & Dewpoint Depression"))
  dev.off()
 
}

writeDB.GEMRegional.soundingData <- function(poi="Lake Louise",type="resorts",limit.data=FALSE, soundingData){
  
  if(missing(soundingData)){
  soundingData <- get.GEMRegional.soundingData(poi = poi, type = type, limit.data = limit.data)
  }
  
  data.to.save <- cbind(location=poi,soundingData$plot.data)
  data.to.save$fcst_date <- min(data.to.save$DATE)
  
  con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
  
  for(j in 1:nrow(data.to.save)){
    sql<-sprintf("insert into ec_gem_regional_sounding (id,`location`,`date`,`press`,`hgt`,`temp`,`depr`,apcp,dewpt,precip,`freeze.level`,`inversion.flag`,`fcst.date`) values (null,'%s','%s',%f,%f,%f,%f,%f,%f,%f,%f,%f, '%s');",data.to.save$location[j],data.to.save$DATE[j],data.to.save$press[j],data.to.save$HGT[j],data.to.save$temp[j],data.to.save$DEPR[j],data.to.save$APCP[j],data.to.save$dewpt[j],data.to.save$precip[j],data.to.save$freeze.level[j],data.to.save$inversion.flag[j],data.to.save$fcst_date[j]) 
    rs<-dbSendQuery(con,sql)  
  }  
  
  dbDisconnect(con)
  rm(con)
  
}
