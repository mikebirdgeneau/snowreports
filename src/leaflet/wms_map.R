library(leaflet)
library(htmltools)
library(htmlwidgets)
library(scales)

library(ProjectTemplate)
reload.project()

safeLabel <- function(label, data) {
  if (is.null(label)) {
    return(label)
  }
  
  label <- evalFormula(label, data)
  if(! (inherits(label, "html") ||
        sum(sapply(label,function(x){!inherits(x,'html')})) == 0)) {
    label <- htmltools::htmlEscape(label)
  }
  label
}

poi.towns <- poi.towns[which(poi.towns@data$Resort!="Calgary NW"),]
poi.towns.cities <- rbind(poi.towns,poi.cities)

shp_bulletins <- suppressWarnings(readOGR("shapefiles/bulletin_regions.shp","bulletin_regions",verbose = FALSE,pointDropZ = TRUE))
shp_bulletins <- spTransform(shp_bulletins,CRS("+proj=longlat +datum=NAD83"))


m <- leaflet() %>% 
  setView(lng = -120, lat = 52, zoom = 5) %>% addProviderTiles("Esri.NatGeoWorldMap",group = "Terrain") %>%
  addPolygons(data = shp_bulletins,
    stroke = TRUE, fillOpacity = 0.6, smoothFactor = 0.5,
    color = ~colorFactor("Paired", levels = shp_bulletins$Name)(Name),
    group = "Bulletin Regions"
  ) %>%
  addWMSTiles(baseUrl = "http://geo.weather.gc.ca/geomet/",
              layers = "HRDPS.CONTINENTAL_NT",
              options = WMSTileOptions(format = "image/png", transparent = TRUE,version = "2.2",styles="CLOUD",opacity=0.7),
              attribution = "Weather data: Environment Canada",
              group="Cloud Cover"
  ) %>% 
  addWMSTiles(baseUrl = "http://geo.weather.gc.ca/geomet/",
              layers = "HRDPS.CONTINENTAL_SN",
              options = WMSTileOptions(format = "image/png", transparent = TRUE,version = "2.2",styles="PRECIPSNOW",opacity=0.7),
              attribution = "Weather data: Environment Canada",
              group="Snowfall"
  ) %>% 
  addWMSTiles(baseUrl = "http://geo.weather.gc.ca/geomet/",
              layers = "HRDPS.CONTINENTAL_UU",
              options = WMSTileOptions(format = "image/png", transparent = TRUE,version = "2.2",styles="WINDARROWKMH",opacity=0.7),
              attribution = "Weather data: Environment Canada",
              group="Wind"
  ) %>% 
  hideGroup(group = "Wind") %>% 
  addWMSTiles(
    baseUrl = "http://geo.weather.gc.ca/geomet/",
    layers = "RADAR_RSNO",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,version = "2.2",styles="RADARURPPRECIPS",opacity=0.7),
    attribution = "Weather Data: Environment Canada",
    group="Radar"
  ) %>% 
  hideGroup(group = "Radar") %>%
  addMarkers(lng = poi.resorts@coords[,1],lat = poi.resorts@coords[,2],popup = htmlEscape(poi.resorts$Resort),group = "Resorts") %>% 
  hideGroup(group = "Backcountry") %>%
  addMarkers(lng = poi.backcountry@coords[,1],lat = poi.backcountry@coords[,2],popup = htmlEscape(poi.backcountry$Resort),group = "Backcountry") %>%
  addMarkers(lng = poi.towns.cities@coords[,1],lat = poi.towns.cities@coords[,2],popup = htmlEscape(poi.towns.cities$Resort),group = "Towns & Cities") %>% 
  hideGroup(group = "Towns & Cities") %>%
  
  addLayersControl(
    baseGroups = c("Terrain"),
    overlayGroups = c("Cloud Cover", "Snowfall", "Radar", "Wind", "Bulletin Regions","Resorts","Backcountry","Towns & Cities"),
    options = layersControlOptions(collapsed = FALSE)
  )

setwd("output/maps")
saveWidget(m, file="weather.html")
setwd("../../")
