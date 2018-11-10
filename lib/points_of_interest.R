# Load Points of Interest data
suppressPackageStartupMessages(library(sp))

# Ski Resorts
poi.resorts<-data.frame(
  # Western Canada
  c("Apex",-119.901258,49.383206),
  c("Big White",-118.936465,49.718226),
  c("Castle",-114.414854,49.318039),
  c("Fernie",-115.088050,49.462611),
  c("Fortress",-115.198479,50.824895),
  c("Kicking Horse",-117.078433,51.274922),
  c("Kimberley",-116.016485,49.682298),
  c("Lake Louise",-116.123127,51.454602),
  c("Marmot",-118.084851,52.800858),
  c("Mt Washington",-125.310683,49.741935),
  c("Mount Cain",-126.343720,50.228459),
  c("Nakiska",-115.151383,50.942581),
  c("Norquay",-115.600661,51.203489),
  c("Panorama",-116.237474,50.457003),
  c("Powder King",-122.613350,55.358821),
  c("Red Mountain",-117.822502,49.104072),
  c("Revelstoke",-118.092132,50.962857),
  c("Shames",-128.961203,54.488983),
  c("Silver Star",-119.061755,50.358843),
  c("Sun Peaks",-119.885167,50.882157),
  c("Sunshine",-115.781810,51.079066),
  c("Whistler",-122.953433,50.107467),
  c("Whitewater",-117.145346,49.443357),
  c("Whitefish",-114.353560,48.486294),
  c("Cypress Mountain",-123.201052,49.395874),
  c("Canada Olympic Park",-114.216618,51.079578),
  c("Purden",-121.907511,53.907447),
  # Eastern Canada
  c("Blue Mountain",-80.315262, 44.502440),
  c("Le Massif",-70.596300,47.279176),
  c("Mont Tremblant",-74.558807, 46.220295),
  c("Mont Saint-Anne",-70.917911,47.083882),
  # Yukon
  c("Mount Sima",-135.052017,60.608052)
)

rownames(poi.resorts)<-c("Resort","x","y")
colnames(poi.resorts)<-NULL
poi.resorts<-data.frame(t(poi.resorts))
poi.resorts$x=as.numeric(as.character(poi.resorts$x))
poi.resorts$y=as.numeric(as.character(poi.resorts$y))
coordinates(poi.resorts)<-c('x','y')
proj4string(poi.resorts)='+proj=longlat +ellps=GRS80 +towgs84'

poi.backcountry<-data.frame(
  c("Rogers Pass",-117.52000,51.300418),
  c("Black Prince",-115.2367,50.706977),
  c("Bow Summit",-116.498122,51.716055),
  c("Highwood Pass",-114.967028,50.594155),
  c("Kootenay Pass",-117.040151,49.056480),
  c("Asulkan",-117.463,51.212),
  c("Balu Pass",-117.564969,51.289535),
  c("Amiskwi Lodge",-116.670556,51.618070),
  c("Pemberton Icecap",-123.2646,50.3568),
  c("Waterton",-114.025,49.0597),
  c("Jumbo Glacier",-116.5611,50.4071),
  c("Burstall Pass",-115.3471,50.772684),
  c("Aster Lake",-115.204170,50.580097),
  c("Assiniboine",-115.6204,50.913072),
  c("Vermillion Peak",-116.1285,51.1568),
  c("Parker Ridge",-117.1298,52.18588),
  c("Little Yoho",-116.5643,51.5256),
  c("Lake OHara",-116.3444,51.3556),
  c("Moraine Lake",-116.180802,51.327815),
  c("Blanket Glacier",-118.248424,50.798957),
  c("Emerald Lake",-116.535424,51.440675),
  c("Maligne Lake",-117.619954,52.725637),
  c("Moose Mountain",-114.838485,50.935745)
)
rownames(poi.backcountry)<-c("Resort","x","y")
colnames(poi.backcountry)<-NULL
poi.backcountry<-data.frame(t(poi.backcountry))
poi.backcountry$x=as.numeric(as.character(poi.backcountry$x))
poi.backcountry$y=as.numeric(as.character(poi.backcountry$y))
coordinates(poi.backcountry)<-c('x','y')
proj4string(poi.backcountry)='+proj=longlat +ellps=GRS80 +towgs84'


poi.cities<-data.frame(
  c("Calgary",-114.081533,51.053464),
  c("Vancouver",-123.113927,49.261226),
  c("Edmonton",-113.5006,53.5472))

rownames(poi.cities)<-c("Resort","x","y")
colnames(poi.cities)<-NULL
poi.cities<-data.frame(t(poi.cities))
poi.cities$x=as.numeric(as.character(poi.cities$x))
poi.cities$y=as.numeric(as.character(poi.cities$y))
coordinates(poi.cities)<-c('x','y')
proj4string(poi.cities)='+proj=longlat +ellps=GRS80 +towgs84'

poi.towns<-data.frame(
  c("Canmore",-115.357831,51.085274),
  c("Banff",-115.570893,51.178401),
  c("Calgary NW",-114.157230,51.111759),
  c("Pincher Creek",-113.9484,49.50099))
rownames(poi.towns)<-c("Resort","x","y")
colnames(poi.towns)<-NULL
poi.towns<-data.frame(t(poi.towns))
poi.towns$x=as.numeric(as.character(poi.towns$x))
poi.towns$y=as.numeric(as.character(poi.towns$y))
coordinates(poi.towns)<-c('x','y')
proj4string(poi.towns)='+proj=longlat +ellps=GRS80 +towgs84'
