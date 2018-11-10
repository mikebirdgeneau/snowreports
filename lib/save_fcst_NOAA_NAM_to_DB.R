writeDB.NAM.local<-function(local){
  require(RMySQL)
  require(data.table)
  model <- nc_open("cache/noaa_nam/noaa_nam.nc",readunlim=FALSE)
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.lat<-local$y[1]
  tmp.long<-local$x[1]
  tmp.height<-round(ncvar_get(model,"HGT_surface",start=c(local$col,local$row,1),count=c(1,1,1)),0)
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
  
  # Generate Plots
  plot.data<-data.table(
    resort=local$resort,
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
    winddir=tmp.winddir
  )
  rm(tmp.apcp,tmp.clouds,tmp.date,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir)
  
  plot.data[,fcst.day:=as.numeric((as.Date(date,tz = "America/Edmonton")-min(as.Date(date,tz = "America/Edmonton")))+1),]
  
  # Generate summary table for storing in database
  
  daily.data<-plot.data[,list(min.date=min(date),max.date=max(date),temp.low=min(temperature),temp.high=max(temperature),snow=sum(snow),rain=sum(rain),pcp=sum(apcp),cloud.avg=mean(clouds),wind.avg=mean(wind),wind.max=max(wind),wind.dir=mean(winddir)),by=c("resort","fcst.day")]
  
  con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
  for(j in 1:nrow(daily.data)){
    sql<-sprintf("insert into namforecast (id,resort,`dmin`,`dmax`,`fcst.day`,`temp.low`,`temp.high`,snow,rain,pcp,`cloud.avg`,`wind.avg`,`wind.max`,`wind.dir`) values (null,'%s','%s','%s',%1d,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.0f);",daily.data$resort[j],daily.data$min.date[j],daily.data$max.date[j],round(daily.data$fcst.day[j],0),daily.data$temp.low[j],daily.data$temp.high[j],daily.data$snow[j],daily.data$rain[j],daily.data$pcp[j],daily.data$cloud.avg[j],daily.data$wind.avg[j],daily.data$wind.max[j],daily.data$wind.dir[j]) 
    rs<-dbSendQuery(con,sql)  
  }  

  on.exit(dbDisconnect(con))
}
