message("[Plotting Resort Soundings.]")
for(i in 1:nrow(poi.resorts)){ 
  message(poi.resorts$Resort[i])
  plot.GEMRegional.sounding(poi=poi.resorts$Resort[i],type="resorts")
}  

for(i in 1:nrow(poi.backcountry)){ 
  message(poi.backcountry$Resort[i])
  plot.GEMRegional.sounding(poi=poi.backcountry$Resort[i],type="backcountry")
}  

for(i in 1:nrow(poi.towns)){ 
  message(poi.towns$Resort[i])
  plot.GEMRegional.sounding(poi=poi.towns$Resort[i],type="towns")
}  

for(i in 1:nrow(poi.cities)){ 
  message(poi.cities$Resort[i])
  plot.GEMRegional.sounding(poi=poi.cities$Resort[i],type="cities")
}  