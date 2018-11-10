require(data.table)

# Load Data
load("output/kml_data.Rda")

# Set-up function for directions
getDir<-function(deg)
{
  dirs<-c(rep("N",times=23),rep("NE",times=45),rep("E",times=45),rep("SE",times=45),
          rep("S",times=45),rep("SW",times=45),rep("W",times=45),rep("NW",times=45),
          rep("N",times=23))
  return(dirs[round(deg,0)])
}

plot.kml<-data.table(plot.data)
plot.kml<-plot.kml[,list(
    date=min(date),
    lat=mean(lat),
    long=mean(long),
    low.temp=round(min(temperature),0),
    max.temp=round(max(temperature),0),
    snow=round(max(snow),1),
    rain=round(max(rain),1),
    apcp=round(max(apcp),1),
    clouds=paste0(round(mean(clouds),2)*100,"%"),
    wind=round(mean(wind),2),
    winddir=round(median(winddir),0),
    press=paste0(round(mean(press),0),"kPa")
    ),by=c("resort")]

coordinates(plot.kml)=~long+lat
proj4string(plot.kml)<-CRS('+proj=longlat')
plot.kml
plotkml<-spTransform(plot.kml,CRS('+proj=longlat +datum=WGS84'))

snowpred <- test2[3]
snowpred@data$snow.acc[which(snowpred@data$snow.acc<=0.999)]<-NA
proj4string(snowpred)<-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=247 +k=90 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs')

plotkml@data$date<-format(as.POSIXct(plotkml@data$date-3600, tz="America/Edmonton"),"%Y-%m-%d %H:%M",tz="America/Edmonton")

# Generate HTML table for each resort
plotkml@data$htmltable<-NA
for(i in 1:nrow(plotkml))
{
  plotkml@data$htmltable[i]=paste0(
    "<h2>",plotkml@data$resort[i],"</h2><table>
    <tr><td>Date:</td><td>",plotkml@data$date[i],"</td></tr>
    <tr><td>Forecast Low:</td><td>",plotkml@data$low.temp[i],"</td></tr>
    <tr><td>Forecast High:</td><td>",plotkml@data$max.temp[i],"</td></tr>
    <tr><td>Forecast Snow:</td><td>",plotkml@data$snow[i],"</td></tr>
    <tr><td>Forecast Rain:</td><td>",plotkml@data$rain[i],"</td></tr>
    <tr><td>Cloud Cover:</td><td>",plotkml@data$clouds[i],"</td></tr>
    <tr><td>Pressure:</td><td>",plotkml@data$press[i],"</td></tr>
    <tr><td>Wind:</td><td>",plotkml@data$wind[i]," ",getDir(plotkml@data$winddir[i]),"</td></tr>
    <tr><td colspan=2><a href='http://snow-reports.ca/weather/location/",str_replace_all(plotkml@data$resort[i]," ","_"),"/'>Details...</a></td></tr></table>")
}

# Set-up Colour Palette for Snow
snowPalette<-colorRampPalette(c("#FFFFFF", "#C5E9FF", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598","#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142"))(1000)
snowColours<-snowPalette[round((seq(20,80,by=1))^2/6.4,0)]
snowColours[1]<-"#FFFFFF00"

plotKML.env(silent=TRUE)
kml_open(file.name="output/snowreports.kml",overwrite=TRUE,folder.name="Snow-Reports.ca")
kml_layer(plotkml,points_names=plotkml@data$resort,html.table=plotkml@data$htmltable,shape="http://maps.google.com/mapfiles/kml/shapes/snowflake_simple.png",size=0.7,colour="white",folder.name="Resorts")
kml_layer.SpatialPixels(snowpred,colour=snow.acc,plot.legend=TRUE,colour_scale=snowColours,balloon=FALSE,folder.name="Snowfall",alpha=1)
kml_close("output/snowreports.kml")
kml_compress(file.name="output/snowreports.kml")


