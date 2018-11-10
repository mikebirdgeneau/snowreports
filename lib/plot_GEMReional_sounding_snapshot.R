plot.GEMRegional.sounding.snapshot<-function(poi="Lake Louise",type="resorts",limit.data=FALSE)
{
  require(animation)
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
  
  plot.data[,dewpt:=temp-DEPR,]
  
  plot.summary<-plot.data[,list(HGT=mean(HGT,na.rm=TRUE),temp=mean(temp,na.rm=TRUE),DEPR=mean(DEPR,na.rm=TRUE)),by=c("press")]
  plot.summary[,dewpt:=temp-DEPR,]
  mean.press.sfc<-mean(tmp.press.sfc*10,na.rm=TRUE) #hPa

  approx.surf<-approx(x=plot.summary$press,y=plot.summary$HGT,xout=mean.press.sfc)$y
  approx.fl<-approx(x=plot.summary$temp,y=plot.summary$HGT,xout=0)$y
  if(!is.finite(approx.fl)){approx.fl<-lm(HGT~temp,data=plot.summary[1:2,])$coefficients[[1]]}
  if(approx.fl<0){approx.fl=0}
  
  p<-ggplot(data=plot.summary)+
    geom_path(aes(y=HGT,x=temp,colour="Temperature (C)"))+
    geom_path(aes(y=HGT,x=dewpt,colour="Dewpoint (C)"),lty=2)+
    geom_vline(x=0,aes(colour="red"),colour="red",lty=3)+
    geom_hline(y=approx.surf,colour="seagreen",lty=3)+
    geom_hline(y=approx.fl,colour="red",lty=1)+
    geom_hline(y=0,colour="lightgrey",lwd=1)+
    #geom_text(x=0,y=approx.surf,label="Surface",size=3,hjust=1.1,vjust=0)+
    theme_bw()+
    scale_colour_manual(values=c("Dewpoint (C)"="brown",
                                 "Temperature (C)"="black",
                                 "Surface"="darkgrey",
                                 "Freezing"="red"),name="")+
    xlab("Temperature / Dewpoint (Celsius)\r\nSnow-Reports.ca - Data: Environment Canada")+
    ylab("Elevation (mASL)")+
    ggtitle(paste0(poi,": Temperature vs. Elevation Profile"))+
    theme(legend.position="bottom")
  
  png(filename=paste0("graphs/48h_",poi,"_sounding.png"),width=560,height=560)
  print(p)
  dev.off()
  
  if(1==0){
  png(filename=paste0("tmp/48h_",poi,"_sounding%03d.png"),width=560,height=560)
  for(i in 1:length(unique(plot.data$DATE)))
  {
    cur.date<-unique(plot.data$DATE)[i]
    cur.data<-plot.data[DATE==cur.date,]
      
      # Find Geopotential Height for Surface
      approx.surf<-approx(x=cur.data$press,y=cur.data$HGT,xout=mean.press.sfc)$y
      approx.fl<-approx(x=cur.data$temp,y=cur.data$HGT,xout=0)$y
      if(!is.finite(approx.fl)){approx.fl<-lm(HGT~temp,data=cur.data[1:2,])$coefficients[[1]]}
      if(approx.fl<0){approx.fl=0}
    
      p<-ggplot(data=cur.data)+
        geom_path(aes(y=HGT,x=temp,colour="Temperature (C)"))+
        geom_path(aes(y=HGT,x=dewpt,colour="Dewpoint (C)"),lty=2)+
        geom_vline(x=0,aes(colour="red"),colour="red",lty=3)+
        geom_hline(y=approx.surf,colour="seagreen",lty=3)+
        geom_hline(y=approx.fl,colour="red",lty=1)+
        geom_hline(y=0,colour="lightgrey",lwd=1)+
        #geom_text(x=0,y=approx.surf,label="Surface",size=3,hjust=1.1,vjust=0)+
        theme_bw()+
        scale_colour_manual(values=c("Dewpoint (C)"="brown",
                                     "Temperature (C)"="black",
                                     "Surface"="darkgrey",
                                     "Freezing"="red"),name="")+
        xlab("Temperature / Dewpoint (Celsius)\r\nSnow-Reports.ca - Data: Environment Canada")+
        ylab("Elevation (mASL)")+
        ggtitle(paste0(poi,": Temperature vs. Elevation Profile \r\nForecast at ",cur.date))+
        theme(legend.position="bottom")
      
      print(p)
    }
  dev.off()
  paste0("tmp/48h_",poi,"_sounding*.png")
  system(paste0("convert -delay 35 -loop 50 tmp/48h_",poi,"_sounding*.png graphs/48h_",poi,"_sounding.gif"))
  system(paste0("rm tmp/48h_",poi,"_sounding*.png"))
  }
}
  