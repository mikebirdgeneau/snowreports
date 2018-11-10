# Write forecast data to database

writeDB.HRDPS.local<-function(local){
  require(RMySQL)
  require(data.table)
  require(ncdf)
  model0 <- nc_open("cache/ec_hrdps/ec_hrdps_000.nc")
  model <- nc_open("cache/ec_hrdps/ec_hrdps.nc",readunlim=FALSE)
  tmp.date=as.POSIXct(format(as.POSIXct.numeric(ncvar_get(model,"time",start=c(1),count=c(24)),tz="UTC",origin="1970-01-01"),tz="America/Edmonton"))
  tmp.lat<-local$y[1]
  tmp.long<-local$x[1]
  tmp.height<-ncvar_get(model,"HGT_surface",start=c(local$col[1],local$row[1],1),count=c(1,1,1))
  tmp.temperature<-ncvar_get(model,"TMP_2maboveground",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))-273.15
  tmp.sprate<-ncvar_get(model,"SPRATE_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.rprate<-ncvar_get(model,"RPRATE_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.apcp<-ncvar_get(model,"APCP_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.clouds<-ncvar_get(model,"TCDC_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))/100
  tmp.wind<-ncvar_get(model,"WIND_9950Etalevel",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))*3.6
  tmp.winddir<-ncvar_get(model,"WDIR_9950Etalevel",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))
  tmp.press<-ncvar_get(model,"PRES_surface",start=c(resort.cells$col[i],resort.cells$row[i],1),count=c(1,1,24))/1000
  # Generate Plots
  plot.data<-data.table(
    resort=resort.cells$resort[i],
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
  for(i in 1:nrow(daily.data)){
    sql<-sprintf("insert into ecforecast24 (id,resort,`dmin`,`dmax`,`temp.low`,`temp.high`,snow,rain,pcp,`cloud.avg`,`wind.avg`,`wind.max`,`wind.dir`,pressure,`snow.acc`) values (null,'%s','%s','%s',%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f);",daily.data$resort[i],daily.data$min.date[i],daily.data$max.date[i],daily.data$temp.low[i],daily.data$temp.high[i],daily.data$snow[i],daily.data$rain[i],daily.data$pcp[i],daily.data$cloud.avg[i],daily.data$wind.avg[i],daily.data$wind.max[i],daily.data$wind.dir[i],daily.data$press[i],daily.data$snow.acc[i]) 
    rs<-dbSendQuery(con,sql)  
  }
  on.exit(dbDisconnect(con))
}