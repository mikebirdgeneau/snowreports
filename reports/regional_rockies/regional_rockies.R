# Script to build Rockies Regional Weather

# Knit PDF document
message("[Generating Rockies Regional PDF...]")
sink(file="/dev/null")
require(knitr)
setwd("reports/regional_rockies/")
knit(input="regional_rockies.Rnw",output="regional_rockies.tex",quiet=TRUE)
if(as.numeric(format(Sys.time(),format="%H")<=12)){
system(paste0("pdflatex regional_rockies.tex; sendEmail -f snowbot@snow-reports.ca -t snow-reports.ca_rockies_weather@lists.snow-reports.ca -u \"Rockies Regional Weather Report - ",format(Sys.time(),"%A %b %d, %Y %H:%M"),"\" -m \"Rockies Regional Weather Forecast for ",format(Sys.time(),"%A %b %d, %Y %H:%M"),"\" -a /home/snowreports/snow-reports.ca/reports/regional_rockies/regional_rockies.pdf"))
} else {
  system(paste0("pdflatex regional_rockies.tex"))
}
setwd("../../")
sink()
message("[Done.]")
