getFcstTime<-function()
{
  # Get current date in UTC
  date.time=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y-%m-%d %H:%M %Z",tz="GMT")
  date=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="GMT")
  date.fancy=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="GMT")
  # Get current hour in UTC
  hr=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%H",tz="GMT")
  hr.fancy=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%H:00 %Z",tz="GMT")
  
  # Set time lag for EC processing for data to become available in GRIB format
  time.lag=5
  
  # Select forecast to grab
  if((as.numeric(hr)-time.lag)>=18)
  {
    fcst.hr<-"18"
  } else if((as.numeric(hr)-time.lag)>=6)
  {
    fcst.hr<-"06"
  } else {
    message(paste0("[ - Forecast for ",date.fancy," not yet available. Using yesterday's 18hr forecast.]\n"))
    date=format(as.POSIXct(Sys.time()-24*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="GMT")
    date.fancy=format(as.POSIXct(Sys.time()-24*3600, tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="GMT")
    fcst.hr<-"18"
  }
  list(date,date.fancy,hr,hr.fancy,time.lag,fcst.hr,date.time)
}

fetchHRDPS<-function()
{
  dir.create("cache/ec_hrdps",showWarnings = FALSE,recursive = TRUE)
  if(!checkNewFcst("HRDPS")$fetch)
  {
    cat("[Latest HRDPS West Forecast already downloaded.]\n")
    return(FALSE)
  }
  
  cat("[Fetching HRDPS West Forecast...]\n")
  
  # Get Time
  timeResult<-getFcstTime()
  date<-timeResult[[1]]
  date.fancy<-timeResult[[2]]
  hr<-timeResult[[3]]
  hr.fancy<-timeResult[[4]]
  time.lag<-timeResult[[5]]
  fcst.hr<-timeResult[[6]]
  rm(timeResult)
  
  # Get variable list
  source("data/env_canada/variables_HRDPS.R")
  
  # Built URL for fetching data
  prefix=paste0("http://dd.weatheroffice.gc.ca/model_hrdps/west/grib2/",fcst.hr,"/")
  suffix=".grib2"
  
  # Fetch 00 Hr Forecast
  prefixes<-paste0(prefix,formatC(0,width=3,flag="0"),"/CMC_hrdps_west_")
  suffixes<-paste0("_ps2.5km_",date,fcst.hr,"_P",formatC(0,width=3,flag="0"),"-00",suffix)
  dl.file.list.0<-data.frame(prefixes,suffixes)
  fcst.list.0<-data.frame(y=fcst.list.0)
  dl.file.list.0<-expand.grid(prefixes=dl.file.list.0$prefixes,suffixes=dl.file.list.0$suffixes,y=fcst.list.0$y)
  dl.file.list.0<-paste0(dl.file.list.0$prefixes,dl.file.list.0$y,dl.file.list.0$suffixes)
  
  for(i in 1:length(dl.file.list.0)){system(paste0("wget -q -nd -N -P 'cache/ec_hrdpstmp/' ",dl.file.list.0[i]))}
  
  system(paste0("ls cache/ec_hrdpstmp/*",date,"*.grib2 | xargs cat > cache/ec_hrdps/ec_hrdps_000-",date,".grib2"))
  system("rm -rf cache/ec_hrdpstmp/*.grib2")
  
  # Fetch Non-Zero Hr Forecast  
  prefixes<-paste0(prefix,formatC(seq(1,24,by=1),width=3,flag="0"),"/CMC_hrdps_west_")
  suffixes<-paste0("_ps2.5km_",date,fcst.hr,"_P",formatC(seq(1,24,by=1),width=3,flag="0"),"-00",suffix)
  fcst.list.1<-data.frame(y=fcst.list.1)
  dl.file.list<-data.frame(prefixes,suffixes)
  dl.file.list<-expand.grid(prefixes=dl.file.list$prefixes,suffixes=dl.file.list$suffixes,y=fcst.list.1$y)
  dl.file.list<-paste0(dl.file.list$prefixes,dl.file.list$y,dl.file.list$suffixes)
  
  pb<-txtProgressBar(min=0,max=length(dl.file.list),initial=0,title="Fetching HRDPS West Forecast",style=3)
  for(i in 1:length(dl.file.list)){
    setTxtProgressBar(pb,value=i)  
    system(paste0("wget -q -nd -N -P 'cache/ec_hrdpstmp' ",dl.file.list[i]))  
  }

  # Combine into a single grib2 file
  system(paste0("ls cache/ec_hrdpstmp/*",date,"*.grib2 | xargs cat > cache/ec_hrdps/ec_hrdps-",date,".grib2"))
  system("rm cache/ec_hrdpstmp/*.grib2")
  
  # Convert to NetCDF format
  system(paste0("nice -n 5 wgrib2 cache/ec_hrdps/ec_hrdps_000-",date,".grib2 -netcdf cache/ec_hrdps/ec_hrdps_000.nc"),ignore.stdout=TRUE)
  system(paste0("nice -n 5 wgrib2 cache/ec_hrdps/ec_hrdps-",date,".grib2 -netcdf cache/ec_hrdps/ec_hrdps.nc"),ignore.stdout=TRUE)
  
  # Make Latest available to Tomcat
  file.copy(from = paste0("cache/ec_hrdps/ec_hrdps_000-",date,".grib2"),to = "/var/lib/tomcat7/webapps/ROOT/data/weather/ec_hrdps_000.grib2", overwrite = TRUE)
  file.copy(from = paste0("cache/ec_hrdps/ec_hrdps-",date,".grib2"),to = "/var/lib/tomcat7/webapps/ROOT/data/weather/ec_hrdps.grib2", overwrite = TRUE)
  
  
  # Remove GRIB2 Files (to conserve disk space)
  file.remove(paste0("cache/ec_hrdps/ec_hrdps_000-",date,".grib2"))
  file.remove(paste0("cache/ec_hrdps/ec_hrdps-",date,".grib2"))
  
  cat("[Done Fetching HRDPS West Forecast.]\n")
  
}

fetchGEMRegional<-function()
{
  dir.create("cache/ec_gemregional",showWarnings = FALSE,recursive = TRUE)
  
  if(!checkNewFcst("GEMRegional")$fetch)
  {
    cat("[Latest GEM Regional Forecast already downloaded.]\n")
    return(FALSE)
  }
  
  cat("[Fetching GEM Regional (10km) Forecast...]\n")
  
  require(ncdf)
  
  # Get Time
  timeResult<-getFcstTime()
  date<-timeResult[[1]]
  date.fancy<-timeResult[[2]]
  hr<-timeResult[[3]]
  hr.fancy<-timeResult[[4]]
  time.lag<-timeResult[[5]]
  fcst.hr<-timeResult[[6]]
  rm(timeResult)
  
  # Get variable list
  source("data/env_canada/variables_GEM.R")
  
  # Built URL for fetching data
  prefix=paste0("http://dd.weatheroffice.gc.ca/model_gem_regional/10km/grib2/",fcst.hr,"/")
  suffix=".grib2"
  
  # Fetch zero hr forecast
  prefixes<-paste0(prefix,formatC(0,width=3,flag="0"),"/CMC_reg_")
  suffixes<-paste0("_ps10km_",date,fcst.hr,"_P",formatC(0,width=3,flag="0"),suffix)
  dl.file.list.0<-data.frame(prefixes,suffixes)
  dl.file.list.0<-merge(dl.file.list.0,fcst.list.0,allow.cartesian=TRUE)
  dl.file.list.0<-paste0(dl.file.list.0$prefixes,dl.file.list.0$y,dl.file.list.0$suffixes)
  
  for(i in 1:length(dl.file.list.0)){system(paste0("wget -q -nd -N -P 'cache/ec_gemregionaltmp/' ",dl.file.list.0[i]))}
  
  system(paste0("ls cache/ec_gemregionaltmp/*",date,"*.grib2 | xargs cat > cache/ec_gemregional/ec_gemregional_000-",date,".grib2"))
  system("rm -rf cache/ec_gemregionaltmp/*.grib2")
  
  # Fetch Non-Zero Hr Forecast  
  prefixes<-paste0(prefix,formatC(seq(3,48,by=3),width=3,flag="0"),"/CMC_reg_")
  suffixes<-paste0("_ps10km_",date,fcst.hr,"_P",formatC(seq(3,48,by=3),width=3,flag="0"),suffix)
  dl.file.list<-data.frame(prefixes,suffixes)
  dl.file.list<-merge(dl.file.list,fcst.list.1,allow.cartesian=TRUE)
  dl.file.list<-paste0(dl.file.list$prefixes,dl.file.list$y,dl.file.list$suffixes)
  
  pb<-txtProgressBar(min=0,max=length(dl.file.list),initial=0,title="Fetching GEM Regional Forecast",style=3)
  for(i in 1:length(dl.file.list)){
    setTxtProgressBar(pb,value=i)  
    system(paste0("wget -q -nd -N -P 'cache/ec_gemregionaltmp' ",dl.file.list[i]))  
  }
  
  
  # Combine into a single grib2 file
  system(paste0("ls cache/ec_gemregionaltmp/*",date,"*.grib2 | xargs cat > cache/ec_gemregional/ec_gemregional-",date,".grib2"))
  system("rm cache/ec_gemregionaltmp/*.grib2")
  
  # Fetch Non-Zero Hr Forecast (Extended Variables) 
  prefixes<-paste0(prefix,formatC(seq(3,48,by=3),width=3,flag="0"),"/CMC_reg_")
  suffixes<-paste0("_ps10km_",date,fcst.hr,"_P",formatC(seq(3,48,by=3),width=3,flag="0"),suffix)
  dl.file.list<-data.frame(prefixes,suffixes)
  dl.file.list<-merge(dl.file.list,fcst.list.1.ext,allow.cartesian=TRUE)
  dl.file.list<-paste0(dl.file.list$prefixes,dl.file.list$y,dl.file.list$suffixes)
  
  pb<-txtProgressBar(min=0,max=length(dl.file.list),initial=0,title="Fetching GEM Regional Forecast (Extended)",style=3)
  for(i in 1:length(dl.file.list)){
    setTxtProgressBar(pb,value=i)  
    system(paste0("wget -q -nd -N -P 'cache/ec_gemregionaltmp' ",dl.file.list[i]))  
  }
  
  # Combine into a single grib2 file
  system(paste0("ls cache/ec_gemregionaltmp/*",date,"*.grib2 | xargs cat > cache/ec_gemregional/ec_gemregional_ext-",date,".grib2"))
  system("rm cache/ec_gemregionaltmp/*.grib2")
  
  # Convert to NetCDF format
  system(paste0("nice -n 5 wgrib2 cache/ec_gemregional/ec_gemregional_000-",date,".grib2 -netcdf cache/ec_gemregional/ec_gemregional_000.nc"),ignore.stdout=TRUE)
  system(paste0("nice -n 5 wgrib2 cache/ec_gemregional/ec_gemregional-",date,".grib2 -netcdf cache/ec_gemregional/ec_gemregional.nc"),ignore.stdout=TRUE)
  system(paste0("nice -n 5 wgrib2 cache/ec_gemregional/ec_gemregional_ext-",date,".grib2 -netcdf cache/ec_gemregional/ec_gemregional_ext.nc"),ignore.stdout=TRUE)
  
  # Make Latest available to Tomcat
  file.copy(from = paste0("cache/ec_gemregional/ec_gemregional_000-",date,".grib2"),to = "/var/lib/tomcat7/webapps/ROOT/data/weather/ec_gemregional_000.grib2", overwrite = TRUE)
  file.copy(from = paste0("cache/ec_gemregional/ec_gemregional-",date,".grib2"),to = "/var/lib/tomcat7/webapps/ROOT/data/weather/ec_gemregional.grib2", overwrite = TRUE)
  file.copy(from = paste0("cache/ec_gemregional/ec_gemregional_ext-",date,".grib2"),to = "/var/lib/tomcat7/webapps/ROOT/data/weather/ec_gemregional_ext.grib2", overwrite = TRUE)
  
  # Remove GRIB2 Files (to conserve disk space)
  file.remove(paste0("cache/ec_gemregional/ec_gemregional_000-",date,".grib2"))
  file.remove(paste0("cache/ec_gemregional/ec_gemregional-",date,".grib2"))
  file.remove(paste0("cache/ec_gemregional/ec_gemregional_ext-",date,".grib2"))
  
  
  cat("[Done Fetching GEM Regional (10km) Forecast.]\n")
  
}

flipVertical <- function(x) {
  if (!inherits(x, "SpatialGridDataFrame")) stop("x must be a SpatialGridDataFrame")
  grd <- getGridTopology(x)
  idx = 1:prod(grd@cells.dim[1:2])
  m = matrix(idx, grd@cells.dim[2], grd@cells.dim[1], byrow = TRUE)[grd@cells.dim[2]:1, ]
  idx = as.vector(t(m)) 
  x@data <- x@data[idx, TRUE, drop = FALSE]
  x
}

getFcstTimeGlobal<-function()
{
  # Get current date in UTC
  date.time=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y-%m-%d %H:%M %Z",tz="GMT")
  date=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="GMT")
  date.fancy=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="GMT")
  # Get current hour in UTC
  hr=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%H",tz="GMT")
  hr.fancy=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%H:00 %Z",tz="GMT")
  
  # Set time lag for EC processing for data to become available in GRIB format
  time.lag=5
  
  # Select forecast to grab
  if((as.numeric(hr)-time.lag)>=12)
  {
    fcst.hr<-"12"
  } else if((as.numeric(hr)-time.lag)>=0)
  {
    fcst.hr<-"00"
  } else {
    message(paste0("[ - Forecast for ",date.fancy," not yet available. Using yesterday's 12h forecast.]\n"))
    date=format(as.POSIXct(Sys.time()-24*3600, tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="GMT")
    date.fancy=format(as.POSIXct(Sys.time()-24*3600, tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="GMT")
    fcst.hr<-"12"
  }
  list(date,date.fancy,hr,hr.fancy,time.lag,fcst.hr,date.time)
}

fetchGEMGlobal<-function()
{
  dir.create("cache/ec_gemglobal",showWarnings = FALSE,recursive = TRUE)
  cat("[Fetching GEM Global (25km) Forecast...]\n")
    
  # Get Time
  timeResult<-getFcstTimeGlobal()
  date<-timeResult[[1]]
  date.fancy<-timeResult[[2]]
  hr<-timeResult[[3]]
  hr.fancy<-timeResult[[4]]
  time.lag<-timeResult[[5]]
  fcst.hr<-timeResult[[6]]
  rm(timeResult)
  
  # Get variable list
  source("data/env_canada/variables_GEM_global.R")
  
  # Built URL for fetching data
  prefix=paste0("http://dd.weatheroffice.gc.ca/model_gem_global/25km/grib2/lat_lon/",fcst.hr,"/")
  suffix=".grib2"
  
    
  # Fetch Zero Hour forecast
  # Fetch 00 Hr Forecast
  
  prefixes<-paste0(prefix,formatC(0,width=3,flag="0"),"/CMC_glb_")
  suffixes<-paste0("_latlon.24x.24_",date,fcst.hr,"_P",formatC(0,width=3,flag="0"),suffix)
  dl.file.list.0<-data.frame(prefixes,suffixes)
  dl.file.list.0<-merge(dl.file.list.0,fcst.list.0,allow.cartesian=TRUE)
  dl.file.list.0<-paste0(dl.file.list.0$prefixes,dl.file.list.0$y,dl.file.list.0$suffixes)
  
  for(i in 1:length(dl.file.list.0)){system(paste0("wget -q -nd -N -P 'cache/ec_gemglobaltmp' ",dl.file.list.0[i]))}
  
  system(paste0("ls cache/ec_gemglobaltmp/*",date,"*.grib2 | xargs cat > cache/ec_gemglobal/ec_gemglobal_000-",date,".grib2"))
  system("rm -rf cache/ec_gemglobaltmp/*.grib2")
  
  # Fetch Non-Zero Hr Forecast  
  prefixes<-paste0(prefix,formatC(seq(3,120,by=3),width=3,flag="0"),"/CMC_glb_")
  suffixes<-paste0("_latlon.24x.24_",date,fcst.hr,"_P",formatC(seq(3,120,by=3),width=3,flag="0"),suffix)
  dl.file.list<-data.frame(prefixes,suffixes)
  dl.file.list<-merge(dl.file.list,fcst.list.1,allow.cartesian=TRUE)
  dl.file.list<-paste0(dl.file.list$prefixes,dl.file.list$y,dl.file.list$suffixes)
  
  pb<-txtProgressBar(min=0,max=length(dl.file.list),initial=0,title="Fetching GEM Global Forecast",style=3)
  for(i in 1:length(dl.file.list)){
  setTxtProgressBar(pb,value=i)  
  system(paste0("wget -q -nd -N -P 'cache/ec_gemglobaltmp' ",dl.file.list[i]))  
  }
  
  # Combine into a single grib2 file
  system(paste0("ls cache/ec_gemglobaltmp/*",date,"*.grib2 | xargs cat > cache/ec_gemglobal/ec_gemglobal-",date,".grib2"))
  system("rm cache/ec_gemglobaltmp/*.grib2")
  
  # Convert to NetCDF format
  system(paste0("wgrib2 cache/ec_gemglobal/ec_gemglobal_000-",date,".grib2 -netcdf cache/ec_gemglobal/ec_gemglobal_000.nc"))
  system(paste0("wgrib2 cache/ec_gemglobal/ec_gemglobal-",date,".grib2 -netcdf cache/ec_gemglobal/ec_gemglobal.nc"))
  
  # Make Latest available to Tomcat
  file.copy(from = paste0("cache/ec_gemglobal/ec_gemglobal_000-",date,".grib2"),
            to = "/var/lib/tomcat7/webapps/ROOT/data/weather/ec_gemglobal_000.grib2",overwrite = TRUE)
  file.copy(from = paste0("cache/ec_gemglobal/ec_gemglobal-",date,".grib2"),
            to = "/var/lib/tomcat7/webapps/ROOT/data/weather/ec_gemglobal.grib2",overwrite = TRUE)
  
  # Remove GRIB2 Files (to conserve disk space)
  file.remove(paste0("cache/ec_gemglobal/ec_gemglobal_000-",date,".grib2"))
  file.remove(paste0("cache/ec_gemglobal/ec_gemglobal-",date,".grib2"))
  
  cat("[Done Fetching GEM Global (25km) Forecast.]\n")
  
}
