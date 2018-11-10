# Script to prep data for Banff Forecast PDF
suppressWarnings(require(ProjectTemplate))
reload.project()

# Get Latest Forecast Data
if(checkNewFcst(fcst="GEMRegional")$fetch){
  message("[Fetching GEM Regional Forecast...]")
  fetchGEMRegional()
} else {
  message("[GEM Regional Forecast is Already Updated.]")
}

# Update Regional Maps (if needed)
if(file.info("graphs/48h_forecastGEM_snow.png")$mtime<=file.info("cache/ec_gemregional/ec_gemregional.nc")$mtime){
  message("[Generating 48h Snow Map...]")
  plot.GEMRegional.precip(type="snow",output=TRUE,limit.hr=48)
}
if(file.info("graphs/24h_forecastGEM_snow.png")$mtime<=file.info("cache/ec_gemregional/ec_gemregional.nc")$mtime){
  message("[Generating 24h Snow Map...]")
  plot.GEMRegional.precip(type="snow",output=TRUE,limit.hr=24)
}
if(file.info("graphs/24h_forecastGEM_cloud.png")$mtime<=file.info("cache/ec_gemregional/ec_gemregional.nc")$mtime){
  message("[Generating 24h Cloud Map...]")
  plot.GEMRegional.clouds(limit.hr=24)
}

# Get Local Forecast for Banff
#load(file="output/pdf_banff.Rda")
if(file.info("graphs/48h_forecast_Banff_wind.png")$mtime<=(file.info("cache/ec_gemregional/ec_gemregional.nc")$mtime+60) | !exists("local.data"))
{
  message("[Generating Local Forecast for Banff...]")
  local.cells<-getCells.GEMRegional(type="towns")
  local<-local.cells[which(local.cells$resort=="Banff"),]
  local.data<-plot.GEMRegional.local(poi="Banff",type="towns",limit.data=FALSE)
  local.data$logo.path<-get.weather.logo(local.data,type="towns")
  local.sun<-fetch.sunsriset(poi=local.data$resort[1],type="towns",dateTime=local.data$date)
  local.data$sunrise<-local.sun$sunrise
  local.data$sunset<-local.sun$sunset
  message("[Local Forecast already Updated. Skipping.]")
}


# Find Forecast Interval
#local.data$period<-NULL
#local.data[date>sunrise & hour(date)<=19,period:="today"]
#local.data[date<(max(sunrise)-90) & is.na(period), period:="overnight"]
#local.data[date>(sunset+90) & date<(max(sunrise)-90),period:="overnight"]
#local.data[date<(sunset+90) & is.na(period),period:="tomorrow"]
#local.data[date>(sunset+90) & is.na(period),period:="tomorrow evening"]

local.data$period<-NA
if(hour(min(local.data$date))<5){
  # Morning Forecast (06 UTC)
  local.data$period[3:6]<-"today"
  local.data$period[7:10]<-"overnight"
  local.data$period[11:14]<-"tomorrow"
} else {
  # Evening Forecast (18 UTC)
  local.data$period[1:2]<-"today"
  local.data$period[3:6]<-"overnight"
  local.data$period[7:11]<-"tomorrow"
}


# Condense to forecast summary
local.data.sum<-local.data[!is.na(period),list(
  date=mean(date),
  min.date=min(date),
  max.date=max(date),
  elev=mean(elev),
  min.temp=round(min(temperature),0),
  max.temp=round(max(temperature),0),
  mean.temp=round(mean(temperature),2),
  temp.trend=mean(diff(temperature),0),
  snow.inst=round(sum(snow.inst),1),
  rain.inst=round(max(rain.inst),1),
  wind=round(mean(wind),0),
  max.wind=round(max(wind),0),
  min.wind=round(min(wind),0),
  winddir=mean(winddir),
  dpt=round(mean(dpt),2),
  clouds=round(mean(clouds),2),
  press=round(mean(press),1),
  press.trend=mean(diff(press)),
  sunrise=max(sunrise),
  sunset=min(sunset),
  periods=sum(0*elev+1)),by=c("period")]

local.data.sum$date<-as.POSIXct(local.data.sum$date,origin="1970-01-01",tz="America/Edmonton")

local.data.sum[,relhum:=calc.relhum(tmp=mean.temp,dpt=dpt),]
local.data.sum$icon<-get.weather.logo(local.data.sum) # Note this doesn't copy the data table, so adds cloudy, etc. columns
local.data.sum$winddir.comp<-get.compass.heading(local.data.sum$winddir)
# Formatting data
txt.weather<-function(local.data.sum=local.data.sum){
  local.data.sum[,temp.trend.txt:=ifelse(abs(temp.trend)<0.5,"stable at",ifelse(round(temp.trend,1)>0.0,"rising to ","falling to"))]
  local.data.sum[,press.trend.txt:=ifelse(abs(press.trend)<0.4,"",ifelse(round(press.trend,1)>0.0,", rising",", falling"))]
  local.data.sum[,cloud.txt:=ifelse(clouds<0.05,"Clear",ifelse(clouds<0.25,"Mostly clear",ifelse(clouds<0.5,"Partly cloudy",ifelse(clouds<=0.75,"Mostly cloudy","Cloudy"))))]
  local.data.sum[,precip.phase:=ifelse(snow.inst>0,ifelse(rain.inst>0,"rain / snow showers",ifelse(snow.inst<2,"flurries",ifelse(snow.inst<7.5,"snow","heavy snow"))),ifelse(rain.inst>0,"rain","")),]
  local.data.sum[,windchill:=13.12+0.6215*mean.temp-11.37*wind^0.16+0.3965*mean.temp*wind^0.16,]
  return(TRUE)
}
txt.weather(local.data.sum)

if(hour(min(local.data$date))<5){
  if(min(local.data$temperature[11:12])<local.data.sum[period=="overnight",min.temp])
  {
    local.data.sum[period=="overnight",min.temp:=round(min(local.data$temperature[11:12]),0),]
  }
} else {
  if(min(local.data$temperature[7:8])<local.data.sum[period=="overnight",min.temp])
  {
    local.data.sum[period=="overnight",min.temp:=round(min(local.data$temperature[7:8]),0),]
  }
}

local.data.sum[,forecast:=paste0(cloud.txt,ifelse(precip.phase!=""," with ",""),precip.phase,ifelse(rain.inst==0,paste0(ifelse(snow.inst<0.6,ifelse(snow.inst<=0.05,"",", \\textless1cm"),paste0(" \\mytilde",round(snow.inst,0),"cm"))),paste0(" \\mytilde",round(snow.inst+rain.inst),"mm $H_2O$")),"\\newline\\noindent ",
                                 ifelse(
                                   daylight & temp.trend>=0,
                                   paste0("High $",max.temp,"\\degree$"),
                                   ifelse(
                                     daylight & temp.trend<0,
                                     paste0("Temperature ",temp.trend.txt," $",min.temp,"\\degree$"),
                                     ifelse(
                                       !daylight & temp.trend<=0,
                                       paste0("Low $",min.temp,"\\degree$"),
                                       paste0("Temperature ",temp.trend.txt," $",max.temp,"\\degree$")
                                       ))),"C\\newline Wind ",winddir.comp," ",wind,"km/h","\\newline Humidity: ",round(relhum,0),"\\%",
                                 "\\newline Pressure ",press," kPa",press.trend.txt),]
local.data.sum

# Save Data for sheet
save(local.data.sum,local.data,local.cells,file="output/pdf_banff.Rda")

# Knit PDF document
message("[Generating PDF...]")
require(knitr)
setwd("reports/banff/")
knit(input="banff.Rnw",output="banff.tex",quiet=TRUE)
system("pdflatex banff.tex")
setwd("../../")

message("[Done.]")
