library("rvest")
library(data.table)
library(stringr)

# Get Snowfall Data
purden<-read_html("http://www.purden.com/ski/skireport.htm")
updated<-purden %>% html_nodes("body p font em") %>% html_text()

if(length(updated)!=0){

  updated<-updated[[1]]
  updated<-parse_date_time(updated,orders = c("mdyHM","mdy"),truncated=2,tz = "America/Edmonton")
  
  # If updated is today, then add hour...
  #if(as.Date(updated)==Sys.Date()){
  #  updated<-updated+hour(Sys.time())*3600+minute(Sys.time())*60
  #} else {
  #  #updated<-updated+20*3600
  #}
  
  test<-data.table((purden %>% html_nodes("table#table12") %>% html_table(fill=TRUE,header=FALSE))[[1]])
  
  setnames(test,c("Parameter","Value"))
  
  test[,Parameter:=str_replace_all(Parameter,pattern = "[^[:alnum:]]",""),]
  test[,Value:=gsub(pattern = "[^([:alnum:]| |\\-| /)]","",Value,perl=TRUE),]
  test$Value
  test[,Value:=str_replace_all(Value,pattern = "\\r|\\n|\\t|cm| C| ",""),]
  test[,Value:=regmatches(Value,regexpr("(^[0-9]+(\\.[0-9])?)",Value,perl = TRUE)),by=c("Parameter")] # Use first number!
  
  data.table(Time=updated,
             Resort="Purden",
             Date=updated,
             snow12h=NA,
             snow24h=ifelse(length(as.numeric(test[Parameter=="Snowfall"]$Value))==0,NA,as.numeric(test[Parameter=="Snowfall"]$Value)),
             Snow48h=NA,
             Snow7d = NA,
             Base=NA,
             Cum=NA,
             Temp= NA,
             Weather=NA,
             Wind=NA,
             Lifts=NA,
             Runs=NA)
} else {
  message("No Report found for Purden.")
}
