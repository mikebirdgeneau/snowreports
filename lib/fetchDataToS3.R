fetchData.GEMRegional<-function(poi="Lake Louise",type="resorts",limit.data=FALSE)
{
  local.cells<-getCells.GEMRegional(type=type)
  local<-local.cells[which(local.cells$resort==poi),]
  
  model0 <- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional_000.nc"))
  model <- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional.nc"),readunlim=TRUE)
  model.ext<- nc_open(paste0(path,"cache/ec_gemregional/ec_gemregional_ext.nc"),readunlim=TRUE)
  
  #dims=dim(ncvar_get(model0,"LAND_surface"))
  date.fancy=format(as.POSIXct.numeric(min(ncvar_get(model0,"time")),tz="UTC",origin="1970-01-01"),"%Y-%m-%d %H:%M %Z",tz="America/Edmonton")
  
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
  rm(tmp.apcp,tmp.clouds,tmp.press.sl,tmp.rprate,tmp.sprate,tmp.temperature,tmp.wind,tmp.winddir,tmp.dpt)
  resort.data<-data.table(tmp)
  rm(tmp)
  
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
  
  plot.data$resort=poi
  
  return(list(poi=poi,local.data=resort.data,elev.data=plot.data))
  
}

