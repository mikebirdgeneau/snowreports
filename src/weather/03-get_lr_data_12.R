# Get current date in UTC
date=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y%m%d",tz="UTC")
date.fancy=format(as.POSIXct(Sys.time(), tz=format(Sys.time(),"%Z")),"%Y-%m-%d",tz="UTC")
cat(paste0("* Fetching low-res data for: ",date)) 

# Prefix
prefix="http://dd.weatheroffice.gc.ca/model_gem_regional/10km/grib2/12/"
suffix=".grib2"

# Set List of post-fix data to gather
fcst.list.0=c("HGT_SFC_0","LAND_SFC_0","SNOD_SFC_0","WTMP_SFC_0")
fcst.list.1=c("PRATE_SFC_0","APCP_SFC_0","TCDC_SFC_0","TMP_TGL_2","WEASN_SFC_0",
              "WEARN_SFC_0","PRES_SFC_0","DPT_TGL_2","WIND_TGL_10","WDIR_TGL_10")

# Fetch 00 Hr Forecast
dl.list.0=paste0("*",fcst.list.0,"*",date,"*",suffix)
system(paste0("wget -q -nd -N -r -l1 -P 'lr_model_data' -A ",paste(dl.list.0,collapse=",")," ",prefix,"000/"))
system(paste0("ls lr_model_data/*",date,"*.grib2 | xargs cat > results/lrmodel_000-",date,".grib2"))
system("rm lr_model_data/*.grib2")

# Fetch non zero forecasts
dl.list.1=paste0("*",fcst.list.1,"*",date,"*",suffix)
for(i in 1:16){
  j=i*3
  if(j<10){
    system(paste0("wget -q -nd -N -r -l1 -P 'lr_model_data' -A ",paste(dl.list.1,collapse=",")," ",prefix,"00",j,"/"))
  } else {
    system(paste0("wget -q -nd -N -r -l1 -P 'lr_model_data' -A ",paste(dl.list.1,collapse=",")," ",prefix,"0",j,"/"))    
  }
}

# Combine into a single grib2 file:
system(paste0("ls lr_model_data/*",date,"*.grib2 | xargs cat > results/lrmodel-",date,".grib2"))
system("rm lr_model_data/*.grib2")
system(paste0("wgrib2 results/lrmodel_000-",date,".grib2 -netcdf results/lrmodel_000.nc"))
system(paste0("wgrib2 results/lrmodel-",date,".grib2 -netcdf results/lrmodel.nc"))

# Clean-up Env
rm(list=ls())
gc()
