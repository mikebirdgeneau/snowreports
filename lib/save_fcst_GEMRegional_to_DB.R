writeDB.GEMRegional.local<-function(local){
  require(RMySQL)
  require(data.table)
  model0 <- nc_open("cache/ec_gemregional/ec_gemregional_000.nc")
  model <- nc_open("cache/ec_gemregional/ec_gemregional.nc",readunlim=FALSE)
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1),count=c(8)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.lat<-local$y[1]
  tmp.long<-local$x[1]
  tmp.height<-ncvar_get(model0,"HGT_surface",start=c(local$col,local$row,1),count=c(1,1,1))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(local$col,local$row,1),count=c(1,1,8))-273.15
  tmp.sprate<-ncvar_get(model,"SPRATE_surface",start=c(local$col,local$row,1),count=c(1,1,8))
  tmp.rprate<-ncvar_get(model,"RPRATE_surface",start=c(local$col,local$row,1),count=c(1,1,8))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(local$col,local$row,1),count=c(1,1,8))
  tmp.clouds<-ncvar_get(model,"TCDC_surface",start=c(local$col,local$row,1),count=c(1,1,8))/100
  tmp.wind<-ncvar_get(model,"WIND_9850Etalevel",start=c(local$col,local$row,1),count=c(1,1,8))*3.6
  tmp.winddir<-ncvar_get(model,"WDIR_9850Etalevel",start=c(local$col,local$row,1),count=c(1,1,8))
  tmp.press<-ncvar_get(model,"PRES_surface",start=c(local$col,local$row,1),count=c(1,1,8))/1000
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
    winddir=tmp.winddir,
    press=tmp.press
  )
  rm(tmp.apcp,tmp.clouds,tmp.date,tmp.press,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir)
  
  # Generate summary table for storing in database
  daily.data<-plot.data[,list(min.date=min(date),max.date=max(date),temp.low=min(temperature),temp.high=max(temperature),snow=max(snow),rain=max(rain),pcp=max(apcp),cloud.avg=mean(clouds),wind.avg=mean(wind),wind.max=max(wind),wind.dir=mean(winddir),press=mean(press),snow.acc=0),by="resort"]
  
  con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
  for(j in 1:nrow(daily.data)){
    sql<-sprintf("insert into ecforecast48 (id,resort,`dmin`,`dmax`,`fcst.day`,`temp.low`,`temp.high`,snow,rain,pcp,`cloud.avg`,`wind.avg`,`wind.max`,`wind.dir`,pressure,`snow.acc`) values (null,'%s','%s','%s',%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f);",daily.data$resort[j],daily.data$min.date[j],daily.data$max.date[j],1,daily.data$temp.low[j],daily.data$temp.high[j],daily.data$snow[j],daily.data$rain[j],daily.data$pcp[j],daily.data$cloud.avg[j],daily.data$wind.avg[j],daily.data$wind.max[j],daily.data$wind.dir[j],daily.data$press[j],daily.data$snow.acc[j]) 
    rs<-dbSendQuery(con,sql)  
  }  
  # Day 2
  model0 <- nc_open("cache/ec_gemregional/ec_gemregional_000.nc")
  model <- nc_open("cache/ec_gemregional/ec_gemregional.nc",readunlim=FALSE)
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(8),count=c(8)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.lat<-local$y[1]
  tmp.long<-local$x[1]
  tmp.height<-ncvar_get(model0,"HGT_surface",start=c(local$col,local$row,1),count=c(1,1,1))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(local$col,local$row,8),count=c(1,1,8))-273.15
  tmp.sprate<-ncvar_get(model,"SPRATE_surface",start=c(local$col,local$row,8),count=c(1,1,8))
  tmp.rprate<-ncvar_get(model,"RPRATE_surface",start=c(local$col,local$row,8),count=c(1,1,8))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(local$col,local$row,8),count=c(1,1,8))
  tmp.clouds<-ncvar_get(model,"TCDC_surface",start=c(local$col,local$row,8),count=c(1,1,8))/100
  tmp.wind<-ncvar_get(model,"WIND_9850Etalevel",start=c(local$col,local$row,8),count=c(1,1,8))*3.6
  tmp.winddir<-ncvar_get(model,"WDIR_9850Etalevel",start=c(local$col,local$row,8),count=c(1,1,8))
  tmp.press<-ncvar_get(model,"PRES_surface",start=c(local$col,local$row,8),count=c(1,1,8))/1000
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
    winddir=tmp.winddir,
    press=tmp.press
  )
  rm(tmp.apcp,tmp.clouds,tmp.date,tmp.press,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir)
  
  # Generate summary table for storing in database
  daily.data<-plot.data[,list(min.date=min(date),max.date=max(date),temp.low=min(temperature),temp.high=max(temperature),snow=max(snow),rain=max(rain),pcp=max(apcp),cloud.avg=mean(clouds),wind.avg=mean(wind),wind.max=max(wind),wind.dir=mean(winddir),press=mean(press),snow.acc=0),by="resort"]
  
  #con<-dbConnect(MySQL(),user="snowreports",password="dbpass",dbname="snowfall",host="localhost")
  for(j in 1:nrow(daily.data)){
    sql<-sprintf("insert into ecforecast48 (id,resort,`dmin`,`dmax`,`fcst.day`,`temp.low`,`temp.high`,snow,rain,pcp,`cloud.avg`,`wind.avg`,`wind.max`,`wind.dir`,pressure,`snow.acc`) values (null,'%s','%s','%s',%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f);",daily.data$resort[j],daily.data$min.date[j],daily.data$max.date[j],2,daily.data$temp.low[j],daily.data$temp.high[j],daily.data$snow[j],daily.data$rain[j],daily.data$pcp[j],daily.data$cloud.avg[j],daily.data$wind.avg[j],daily.data$wind.max[j],daily.data$wind.dir[j],daily.data$press[j],daily.data$snow.acc[j]) 
    rs<-dbSendQuery(con,sql)  
  }
  on.exit(dbDisconnect(con))
}
