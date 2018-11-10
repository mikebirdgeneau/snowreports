
getNAMFcstDate <- function(){
  require("rvest")
  require("stringr")
  library("data.table")
  
  # Clean-up old GRIB files:
  unlink(list.files(path = "cache/noaa_nam/",pattern = "*.grib2",full.names = TRUE))
  
  nam.index<-html("http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/") %>% html_nodes("pre a") %>% html_text()
  nam.index<-str_replace_all(nam.index,"/","")
  #nam.index<-paste0(nam.index, collapse = " ")
  nam.index<-regmatches(nam.index,regexpr(pattern = "(?<=nam\\.)[0-9]+",text = nam.index,perl=TRUE))
  max.date<-max(strptime(nam.index,format = "%Y%m%d",tz = "UTC"))
 
  nam.index.files<-html(paste0("http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/nam.",format(max.date,format="%Y%m%d"),"/")) %>% html_nodes("pre a") %>% html_text()
  
  run.times<-data.table(
    Run=c(regmatches(nam.index.files,regexpr(pattern = "(?<=nam\\.t)[0-9][0-9](?=z.awphys.*tm[0-9][0-9]$)",text = nam.index.files,perl=TRUE))),
    File=c(regmatches(nam.index.files,regexpr(pattern = "nam\\.t[0-9][0-9]z.awphys.*tm[0-9][0-9]$",text = nam.index.files,perl=TRUE))))
  run.times[,id:=seq(1,.N,by=1),]
  run.time.summary<-run.times[,list(Count=sum(0*as.numeric(id)+1)),by=c("Run")]
  last.full.run<-max(run.time.summary[Count==max(run.time.summary$Count),]$Run)
  
  return(list(date=format(max.date,format="%Y%m%d",tz="UTC"),
              last.full.run = last.full.run,run.times=run.times)
         )
}

bdown=function(url, file, showprogress=TRUE){
  library('RCurl')
  f = CFILE(file, mode="wb")
  #a = curlPerform(url = url, writedata = f@ref, noprogress=!showprogress)
  a = try(curlPerform(url = url, writedata = f@ref, noprogress=!showprogress))  
  if (class (a) == "try-error") {
    a = try (curlPerform (url = url, writedata = f@ref, noprogress=!showprogress))
    if (class (a) == "try-error")
      a = paste ("error accessing", url)
  }  
  
  close(f)
  return(a)
}

getNAMForecast <- function(fcst.info=getNAMFcstDate(),leftlon=-160,rightlon=-110,toplat=80,bottomlat=44)
{
  file.list<-fcst.info$run.times[Run == fcst.info$last.full.run,]$File
  #leftlon=-160
  #rightlon=-110
  #toplat=80
  #bottomlat=44
  message(paste0("Temp Dir: ",tempdir()))
  
  # Download files:
  
  lapply(file.list,function(x){
    
    url<-paste0("http://nomads.ncep.noaa.gov/cgi-bin/filter_nam.pl?file=",x,"&lev_0C_isotherm=on&lev_1000_mb=on&lev_150_mb=on&lev_200_mb=on&lev_250_mb=on&lev_2_m_above_ground=on&lev_300_mb=on&lev_325_mb=on&lev_350_mb=on&lev_375_mb=on&lev_400_mb=on&lev_425_mb=on&lev_450_mb=on&lev_475_mb=on&lev_500_mb=on&lev_525_mb=on&lev_550_mb=on&lev_575_mb=on&lev_600_mb=on&lev_625_mb=on&lev_650_mb=on&lev_675_mb=on&lev_700_mb=on&lev_725_mb=on&lev_750_mb=on&lev_775_mb=on&lev_800_mb=on&lev_825_mb=on&lev_850_mb=on&lev_875_mb=on&lev_900_mb=on&lev_925_mb=on&lev_950_mb=on&lev_975_mb=on&lev_entire_atmosphere_%5C%28considered_as_a_single_layer%5C%29=on&lev_lowest_level_of_the_wet_bulb_zero=on&lev_max_wind=on&lev_surface=on&var_APCP=on&var_ACPCP=on&var_CFRZR=on&var_CICEP=on&var_CRAIN=on&var_CSNOW=on&var_GUST=on&var_HGT=on&var_PRES=on&var_PRMSL=on&var_PWAT=on&var_RH=on&var_SNOD=on&var_TCDC=on&var_TMP=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=",leftlon,"&rightlon=",rightlon,"&toplat=",toplat,"&bottomlat=",bottomlat,"&dir=%2Fnam.",fcst.info$date)
    message(paste0("Fetching file ",file.list[which(x == file.list)]))
    if(!file.exists(paste0(tempdir(),"/",x,".grib2"))){
    bdown(url,paste0(tempdir(),"/",x,".grib2"),showprogress=TRUE)
    } else {
      message(paste0(x,".grib2 Already Exists!"))
    }
  })
  
  # Combine files & Convert to NCDF
  
  dir.create(path = "cache/noaa_nam/",showWarnings = FALSE)
  system(paste0("ls ",tempdir(),"/*.grib2 | xargs cat > cache/noaa_nam/nam_",fcst.info$date,"_",fcst.info$last.full.run,".grib2"))
  system(paste0("nice -n 5 wgrib2 cache/noaa_nam/nam_",fcst.info$date,"_",fcst.info$last.full.run,".grib2 -netcdf cache/noaa_nam/noaa_nam.nc"),ignore.stdout=TRUE)
  
  system(paste0("rm ",tempdir(),"/*.grib2"))

  if(!file.exists("cache/noaa_nam/latlon-g218.txt")){
    download.file("http://nomads.ncdc.noaa.gov/data/nam/latlon-g218.txt",destfile = "cache/noaa_nam/latlon-g218.txt")
  }
  
  
}

# proj.NAM <- "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=265 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"
